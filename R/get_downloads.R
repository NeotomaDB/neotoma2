#' @title Get downloads - Data 
#' @import gtools
#' @param datasetid integer A collection unit ID
#' @param ... arguments in ellipse form
#' @export
get_downloads <- function(datasetid = NA, ...) {
  if(!missing(datasetid)) {
    UseMethod('get_downloads', datasetid)
  } else {
    UseMethod('get_downloads', NA)
  }
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
  for(i in 1:result_length) {
    # i-th element result[2]$data[[i]]$
    coll_units <- c()
    dataset_list <- c()
    
    # Sites 
    # Sitename
    sitename <- result$data[[i]]$record$data$dataset$site$sitename
    
    # Site ID
    siteid <- result$data[[i]]$record$data$dataset$site$siteid
    
    # Location
    location <- st_read(result$data[[i]]$record$data$dataset$site$geography, quiet = TRUE)

    # Altitude
    elev <- result$data[[i]]$record$data$dataset$site$altitude

    # Description 
    description <- result$data[[i]]$record$data$dataset$site$sitedescription

    # Notes
    notes <- NA_character_

  # Datasets

    datasetid <- result$data[[i]]$record$data$dataset$site$collectionunit$dataset$datasetid
    datasettype <- result$data[[i]]$record$data$dataset$site$collectionunit$dataset$datasettype
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
  collunitid <- result$data[[i]]$record$data$dataset$site$collectionunit$collectionunitid

  colldate = as.Date("2007-02-01")

  # Coll Unit Handle
  handle <- result$data[[i]]$record$data$dataset$site$collectionunit$handle

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
  
  # Delete print once it works properly
  #print("Getting in Datasets Numeric")
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
  #Print when debugging
  #print(result)
  output <- parse_download(result)
  
  return(output)
}

