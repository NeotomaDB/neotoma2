#' @title Get downloads - Data 
#' @import gtools
#' @import dplyr
#' @param datasetid integer A collection unit ID
#' @param ... arguments in ellipse form
#' @export
get_downloads <- function(datasetid = NA, ...) {
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
  
  #result <- result %>% fixNull()
  result <- result[2]
  #print(result)
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
    sitename <- result$data[[i]]$site$sitename
    
    # Site ID
    siteid <- result$data[[i]]$site$siteid
    
    # Location
    location <- result$data[[i]]$site$geography
    if(is.null(location)){
      location <- st_sf(st_sfc())
    }else{
      location <- st_read(result$data[[i]]$site$geography, quiet = TRUE)
    }
    
    # Altitude
    elev <- result$data[[i]]$site$altitude
    
    # Description 
    description <- result$data[[i]]$site$sitedescription
    if(is.na(description)){
      description <- NA_character_
    }else{
      description <- result$data[[i]]$site$sitedescription
    }
    
    # Notes
    notes <- NA_character_
    
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
          #first_name <- result$data[[i]]$site$collectionunit$dataset$samples[[k]]$sampleanalyst[[j]]$firstname
          #last_name <- result$data[[i]]$site$collectionunit$dataset$samples[[k]]$sampleanalyst[[j]]$familyname
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
  
  #     # Lab Data
  #     df_lab <- result$data[[i]]$dataset$data$samples[[j]]$datum %>% map(function(x){as.data.frame(x)}) %>% bind_rows() %>%
  #       select(taxonid)
  #     
  #     df_lab1 <- rbind(df_lab, df_lab1) %>%
  #       distinct()
  #   
  #   # Chronologies filtering
  #   wang <- df3 %>%
  #     filter(chronologyname == "Wang et al.")
  #   
  #   cohmap <- df3 %>%
  #     filter(chronologyname == "COHMAP chron 2")
  #   
  #   napd <- df3 %>%
  #     filter(chronologyname == "NAPD 1")
  # }  
  # 
  # } 
  # 
  # # DOUBT TODO : How should I save these tables?
  #print(df_counts)
  #print(df_lab1)
  #print(napd)
  #print(cohmap)
  #print(wang)
  
  # Convert to sites element
  sites <- new('sites', sites = sites) 
  
  #return(result)
  return(sites)
}

#' @title Get Downloads
#' @import lubridate
#' @importFrom methods new
#' @param datasetid Use a single number to extract site information
#' @param ... arguments in ellipse form
#' @export
get_downloads.numeric <- function(datasetid, ...) {
  
  useNA <- function(datasetid, type) {
    if (is.na(datasetid)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(datasetid)
    }
  }
  
  if (length(datasetid) > 0) {
    dataset <- paste0(datasetid, collapse = ',')
  }
  
  baseURL <- paste0('data/downloads/', dataset)
  
  result <- parseURL(baseURL)
  #print(result)
  
  output <- parse_download(result)
  
  #print(class(result))
  
  return(output)
}

