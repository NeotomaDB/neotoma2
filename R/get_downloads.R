#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Information for Fossil Datasets
#' Look for the whole data details using only a dataset ID or for multiple metadata.
#' Displays a site table with the following columns: siteid, sitename, lat, long, and elev.
#' The function takes parameters defined by the user and returns a sites object
#' with more detailed information regarding datasets and samples.
#' The user may define all or none of the possible fields.  
#' The function contains data checks for each defined parameter.
#' @param x Use a single number to extract site information
#' @param ... accepted arguments: sites, datasets
#' @return The function returns either a single item of class \code{"try-error"} describing
#'    the reason for failure (either mis-defined parameters or an error from the Neotoma API),
#'    or a table of sites, with rows corresponding to the number of individual sites and datasets
#'    returned by the Neotoma API.  
#'    Each "site" object contains 6 parameters that can be accessed as well:
#'    siteid, sitename, location, altitude, description, limited collection units information.
#' \item{ \code{siteid} }{site ID number}
#' \item{ \code{sitename} }{site's name}
#' \item{ \code{location} }{sf object that describes site's location}    
#' \item{ \code{description} }{}
#' \item{ \code{collunits} }{limited information on collunits}  
#'   Each "collection unit" embedded in the "sites" object contains 6 parameters that can be accessed as well:
#' \item{ \code{collunitid}}{collection unit ID number}
#' \item{ \code{handle} }{collection unit's handle}
#' \item{ \code{collunitname} }{collection unit's name}    
#' \item{ \code{colldate} }{date in collection unit}
#' \item{ \code{substrate} }{substrate}  
#' \item{ \code{location} }{sf object that describes site's location}    
#' \item{ \code{datasets} }{detailed information regarding dataset}
#'   Each "dataset" nested in the "collection unit" contains the following detail of information:
#' \item{ \code{datasetid} }{dataset ID number}
#' \item{ \code{datasetname} }{site's name}
#' \item{ \code{datasettype} }{type of data found}    
#' \item{ \code{location} }{sf object that describes site's location}
#' \item{ \code{notes} }{notes on the dataset}
#' \item{ \code{taxa table} }{taxa table}    
#' \item{ \code{pi list} }{P.I. info}
#' \item{ \code{analyst} }{analyst info}
#' \item{ \code{metadata} }{dataset metadata}
#' @examples \dontrun{
#' To find the downloads object of dataset 24:
#' downloads24 <- get_downloads(24)
#' 
#' To find all downloads in Brazil
#' brazil <- '{"type": "Polygon", 
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]
#' ]]}'
#' brazil_datasets <- get_datasets(loc = brazil[1])
#' brazil_downloads <- get_downloads(brazil_datasets)
#' }
#' @export
get_downloads <- function(datasetid = NA, ..., complete_data = FALSE, verbose = 0) {
  if(!missing(datasetid)) {
    UseMethod('get_downloads', datasetid)
  } #else {
  #UseMethod('get_downloads', NA)
  #}
}

parse_download <- function(result) {
  fixNull <- function(x) {
    for (i in 1:length(x)) {
      if (is.null(x[[i]])) {
        x[[i]] <- NA
      } else {
        if (class(x[[i]]) == 'list') {
          x[[i]] <- fixNull(x[[i]])
        }
      }
    }
    return(x)
  }
  
  result <- result[2]
  result_length <- length(result$data)
  
  sites <- c()
  taxon_table <- data.frame()
  df_lab1 <- data.frame()
  df_counts <- data.frame()
  for(i in 1:result_length) {
    # i-th element result[2]$data[[i]]$
    coll_units <- c()
    dataset_list <- c()
    
    # Sites 
    # Sitename
    if(is.na(result$data[[i]]$site$sitename)){
      sitename <- NA_character_
    }else{
      sitename <- result$data[[i]]$site$sitename
    }
 
    # Site ID
    if(is.na(result$data[[i]]$site$siteid)){
      siteid <- NA_integer_
    }else{
      siteid <- result$data[[i]]$site$siteid
    }
    
    # Location
    location <- result$data[[i]]$site$geography
    if(is.null(location)){
      location <- st_sf(st_sfc())
    }else{
      location <- st_read(result$data[[i]]$site$geography, quiet = TRUE)
    }
    
    # Altitude
    if(is.na(result$data[[i]]$site$altitude)){
      elev <- NA_integer_
      }else{
    elev <- result$data[[i]]$site$altitude
      }
    
    # Description 
    description <- result$data[[i]]$site$sitedescription
    if(is.na(description)){
      description <- NA_character_
    }else{
      description <- result$data[[i]]$site$sitedescription
    }
    
    # Notes
    if(is.na(result$data[[i]]$site$notes)){
      notes <- NA_character_
    }else{
      notes <- result$data[[i]]$site$notes
    }
    
    # Datasets
    datasetid <- result$data[[i]]$site$collectionunit$dataset$datasetid
    datasettype <- result$data[[i]]$site$collectionunit$dataset$datasettype
    
    datasetnotes <- result$data[[i]]$site$collectionunit$dataset$datasetnotes
    if(is.na(datasetnotes)){
      datasetnotes <- NA_character_
    }else{
      datasetnotes <- result$data[[i]]$site$collectionunit$dataset$datasetnotes
    }
    
    # Taxon Table
    length_datum <- length(result$data[[i]]$site$collectionunit$dataset$samples)
    
    for(j in 1:length_datum){
      depth <- result$data[[i]]$site$collectionunit$dataset$samples[[j]]$depth
      sample_id <-result$data[[i]]$site$collectionunit$dataset$samples[[j]]$sampleid
      dataset_id <- result$dataset$data[[i]]$site$siteid
      df <- result$data[[i]]$site$collectionunit$dataset$samples[[j]]$datum %>%
        map(function(x){as.data.frame(x)}) %>%
        bind_rows()
      
      df_sample <- df %>%
        select(variablename, units, element, taxongroup, ecologicalgroup, taxonid) # uncomment for order selection
      
      taxon_table <- rbind(taxon_table, df_sample) %>%
        distinct()
      
      # PI Information
      pi_length <- length(result$data[[i]]$site$collectionunit$dataset$datasetpi)
      pi_list <- c()
      
      for(j in 1:pi_length){
        pi <- result$data[[i]]$site$collectionunit$dataset$datasetpi[[j]]$contactname
        pi_list <- c(pi_list, pi)}
      
      # Analyst Info
      analyst_list <- list()
      samples_length <- length(result$data[[1]]$site$collectionunit$dataset$samples)
      for(k in 1:samples_length){
        analyst_length <- length(result$data[[i]]$site$collectionunit$dataset$samples[[k]]$sampleanalyst)
        for(j in 1:analyst_length){
          contact_name <- result$data[[i]]$site$collectionunit$dataset$samples[[k]]$sampleanalyst[[j]]$contactname
          analyst_list <- c(analyst_list, contact_name)}
      }
      
      # Sample.Meta Table
      meta_data_t1 <- result$data[[i]]$site$collectionunit$dataset$samples[[j]]$ages %>% 
        map(function(x){as.data.frame(x)}) %>% 
        bind_rows()
      
      meta_data_t1 <- meta_data_t1 %>%
        mutate(
          datasetid = datasetid,
          depth = depth,
          sample.id = sample_id)
      
      if(dim(meta_data_t1)[1] > 0){
        meta_data_t1 %>%
          select("depth", "ageolder", "age", "ageyounger", "chronologyname", "agetype", "chronologyid", "sample.id", "datasetid")
      }
      
      new_dataset <- new('dataset',
                         datasetid = datasetid,
                         datasetname = sitename,
                         datasettype = datasettype,
                         location = location,
                         notes = datasetnotes,
                         taxa_table = taxon_table,
                         pi_list = pi_list,
                         analyst = analyst_list,
                         metadata = meta_data_t1)
      
      
      
      dataset_list <- append(dataset_list, new_dataset)
      datasets_list <- new('datasets', datasets = dataset_list)
      
      # Add to dataset publications information
      # Publications Information
      
      # API endpoint is different
      # https://api.neotomadb.org/v2.0/data/datasets/1/publications
      
      
      # Count Samples metadata
      if (result$data[[i]]$site$collectionunit$dataset$datasettype == 'geochronologic') {
        
        message(paste0('The dataset ID ', result$data[[i]]$site$collectionunit$dataset$datasetid,
                       ' is associated with a geochronology object, not count data.'))
        return(NULL)
        
      } else {
        
        # copy to make indexing below easier?
        samples <- result$data[[i]]$site$collectionunit$dataset$samples
        
        
        # Build the metadata for each sample in the dataset.
        sample.meta <- do.call(rbind.data.frame,
                               lapply(samples, `[`,
                                      c("depth",
                                        "sampleid"
                                      )))
        
      }
      # Counts
      df_count <- df %>%
        select(variablename, value, taxonid)
      
      df_counts <- rbind(df_counts, df_count) %>%
        distinct()
    }
    
    ## Collunits
    # Coll Unit ID
    collunitid <- result$data[[i]]$site$collectionunit$collectionunitid
    
    colldate <- result$data[[i]]$site$collectionunit$colldate
    if(is.na(colldate)){
      colldate <- as.Date("2000-10-10")
    }else{
      colldate <- as.Date(result$data[[i]]$site$collectionunit$colldate)
    }
    
    # Coll Unit Handle
    handle <- result$data[[i]]$site$collectionunit$handle
    
    new_collunit <- new("collunit",
                        collunitid = collunitid,
                        colldate = colldate,
                        handle = handle,
                        datasets = datasets_list)
    
    coll_units <- append(coll_units, new_collunit)
    coll_units <- new('collunits', collunits = coll_units)
    
    new_site <- new("site",
                    siteid = siteid,
                    sitename = sitename,
                    location = location,
                    altitude = elev,
                    description = description,
                    notes = NA_character_,
                    collunits = coll_units)
    
    sites <- append(sites, new_site)
  }
  
  # Convert to sites element
  sites <- new('sites', sites = sites) 

  return(sites)
}

#' @title get_downloads
#' @param datasetid Use a single number to extract site information
#' @param ... arguments in ellipse form
#' @export
get_downloads.numeric <- function(datasetid, ..., complete_data = FALSE, verbose = 0) {
  
  useNA <- function(datasetid, type) {
    if (is.na(datasetid)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(datasetid)
    }
  }
  
  cl <- as.list(match.call())
  
  possible_arguments <- c("offset", "all_data", "sites_o", "verbose", "datasetid")
  
  cl[[1]] <- NULL
  
  for(name in names(cl)){
    if(!(name %in% possible_arguments)){
      message(paste0(name, " is not an allowed argument. Argument will be ignored. Choose from the allowed arguments: sitename, altmax, altmin, loc"))
    }
  }
  
  
  if (length(datasetid) > 0) {
    dataset <- paste0(datasetid, collapse = ',')
  }
  
  baseURL <- paste0('data/downloads/', dataset)
  result <- parseURL(baseURL)
  
  output <- parse_download(result)

  return(output)
}

#' @title get_downloads sites
#' @param sites_o sites object
#' @param ... arguments in ellipse form
#' @export
get_downloads.sites <- function(sites_o, ..., complete_data = FALSE, verbose = 0) {
  
  useNA <- function(datasetid, type) {
    if (is.na(datasetid)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(datasetid)
    }
  }
  
  cl <- as.list(match.call())
  
  possible_arguments <- c("offset", "all_data", "sites_o", "verbose", "datasetid")
  
  cl[[1]] <- NULL
  
  for(name in names(cl)){
    if(!(name %in% possible_arguments)){
      message(paste0(name, " is not an allowed argument. Argument will be ignored. Choose from the allowed arguments: sitename, altmax, altmin, loc"))
    }
  }
  
  dataset_list <- c()
  for(i in 1:length(sites_o)){
    for(j in 1:length(sites_o@sites[[i]]@collunits@collunits)){
      for(k in 1:length(sites_o@sites[[i]]@collunits@collunits[[j]]@datasets@datasets)){
        datasetid <- sites_o@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]@datasetid
        dataset_list <- c(dataset_list, datasetid)
      }
    }
  }
  
  output <- get_downloads(dataset_list, verbose=verbose)
  
  return(output)
}