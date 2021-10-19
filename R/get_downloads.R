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
  pi_list <- c()
  df1 <- data.frame()
  df_lab1 <- data.frame()
  df3 <- data.frame()
  df_counts <- data.frame()
  for(i in 1:result_length) {
    # i-th element result[2]$data[[i]]$
    coll_units <- c()
    dataset_list <- c()
    
    # Sites 
    # Sitename
    sitename <- result$data[[i]]$dataset$data$dataset$site$sitename
    
    # Site ID
    siteid <- result$data[[i]]$dataset$data$dataset$site$siteid
    
    # Location
    location <- result$data[[i]]$dataset$data$dataset$site$geography
    if(is.null(location)){
      location <- st_sf(st_sfc())
    }else{
      location <- st_read(result$data[[i]]$dataset$data$dataset$site$geography, quiet = TRUE)
    }


    # Altitude
    elev <- result$data[[i]]$dataset$data$dataset$site$altitude

    # Description 
    description <- result$data[[i]]$dataset$data$dataset$site$sitedescription
    if(is.na(description)){
      description <- NA_character_
    }else{
      description <- result$data[[i]]$dataset$data$dataset$site$sitedescription
    }

    # Notes
    notes <- NA_character_

  # Datasets
    datasetid <- result$data[[i]]$dataset$data$dataset$site$collectionunit$dataset$datasetid
    datasettype <- result$data[[i]]$dataset$data$dataset$site$collectionunit$dataset$datasettype
    datasetnotes <- NA_character_
    
    new_dataset <- new('dataset',
                       datasetid = datasetid,
                       datasetname = sitename,
                       datasettype = datasettype,
                       location = location,
                       notes = datasetnotes)
  
  dataset_list <- append(dataset_list, new_dataset)
  datasets_list <- new('datasets', datasets = dataset_list)

  ## Collunits
  # Coll Unit ID
  collunitid <- result$data[[i]]$dataset$data$dataset$site$collectionunit$collectionunitid

  colldate = as.Date("2007-02-01")

  # Coll Unit Handle
  handle <- result$data[[i]]$dataset$data$dataset$site$collectionunit$handle

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
  
  
  # PI Information
  #pi_length <- length(result$data[[1]]$dataset$data$dataset$site$collectionunit$dataset$datasetpi[[1]])
  
  # for(j in range(1:pi_length)){
  #   pi <- result$data[[1]]$dataset$data$dataset$site$collectionunit$dataset$datasetpi[[1]]$contactname
  #   pi_list <- append(pi_list, pi)}
  # 
  # 
  # 
  # # Count Samples metadata
  # 
  # if (result$data[[i]]$dataset$data$dataset$site$collectionunit$dataset$datasettype == 'geochronologic') {
  #   
  #   message(paste0('The dataset ID ', dataset$dataset.meta$dataset.id,
  #                  ' is associated with a geochronology object, not count data.'))
  #   return(NULL)
  #   
  # } else {
  #   
  #   # copy to make indexing below easier?
  #   samples <- result$data[[i]]$dataset$data$samples
  #   
  #   
  #   # Build the metadata for each sample in the dataset.
  #   sample.meta <- do.call(rbind.data.frame,
  #                          lapply(samples, `[`,
  #                                 c("depth",
  #                                   "sampleid"
  #                                 )))
  #   
  #   
  #   #samples <- fromJSON(samples)
  #   
  #   #depth <- modify_depth(alex_samples, 1, "depth") %>% as_vector()
  #   
  #   
  #   # Taxon Table
  #   length_datum <- length(result$data[[i]]$dataset$data$samples)
  #   
  #   for(j in 1:length_datum){
  #     depth <- result$data[[i]]$dataset$data$samples[[j]]$depth
  #     sample_id <-result$data[[i]]$dataset$data$samples[[j]]$sampleid
  #     dataset_id <- result$dataset$data[[i]]$dataset$site$siteid
  #     df <- result$data[[i]]$dataset$data$samples[[j]]$datum %>% 
  #       map(function(x){as.data.frame(x)}) %>% 
  #       bind_rows()
  #     
  #     df_sample <- df %>%
  #       select(variablename, units, element, taxongroup, ecologicalgroup, taxonid) # uncomment for order selection
  #     
  #     df1 <- rbind(df1, df_sample) %>%
  #       distinct()
  #      
  #     # Counts
  #     df_count <- df %>%
  #       select(variablename, value, taxonid)
  #     
  #     df_counts <- rbind(df_counts, df_count) %>%
  #       distinct()
  #     
  #     # Lab Data
  #     df_lab <- result$data[[i]]$dataset$data$samples[[j]]$datum %>% map(function(x){as.data.frame(x)}) %>% bind_rows() %>%
  #       select(taxonid)
  #     
  #     df_lab1 <- rbind(df_lab, df_lab1) %>%
  #       distinct()
  #     
  #     # Sample.Meta Table
  #     df2 <- result$data[[i]]$dataset$data$samples[[j]]$ages %>% map(function(x){as.data.frame(x)}) %>% bind_rows()
  #     df2 <- df2 %>%
  #       mutate(
  #         depth = depth,
  #         sample.id = sample_id,
  #         datasetid = dataset_id) %>%
  #       select(depth, ageolder, age, ageyounger, chronologyname, agetype, chronologyid, sample.id, datasetid)
  #     
  #     df3 <- rbind(df3, df2)
  #   }
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
  # #print(pi_list)
  # print(df1)
  # print(df_counts)
  # #print(df_lab1)
  # #print(df3)
  # #print(napd)
  # #print(cohmap)
  # #print(wang)
  
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
  print(result)
  
  output <- parse_download(result)
  
  print(class(result))
  
  return(output)
}

