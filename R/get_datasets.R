#' @title Get datasets - Data Sets Information for Fossil Sites
#' @import gtools
#' @param datasetid integer A collection unit ID
#' @param ... arguments in ellipse form
#' @export
get_datasets <- function(datasetid = NA, ...) {
  if(!missing(datasetid)) {
    UseMethod('get_datasets', datasetid)
  } else {
    UseMethod('get_datasets', NA)
  }
}

parse_dataset <- function(result) {
  now <- Sys.time()
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
  
  result <- result %>% fixNull()
  
  result_length <- length(result[2]$data)
  
  dataset_list <- c()
  
  for(i in 1:result_length) {
    # i-th element result[2]$data[[i]]$
    datasetid <- result[2]$data[[i]]$site$datasets[[1]]$datasetid
    datasetname <- result[2]$data[[i]]$site$sitename
    datasettype <- result[2]$data[[i]]$site$datasets[[1]]$datasettype
    location <- st_read(result[2]$data[[i]]$site$geography, quiet = TRUE)
    note <- NA_character_

    new_dataset <- new('dataset',
                       datasetid = datasetid,
                       datasetname = datasetname,
                       datasettype = datasettype,      
                       location = location,
                       notes = note)
    
    dataset_list <- append(dataset_list, new_dataset)
    
    output <- new('datasets', datasets = dataset_list)
    
  }
  
  after <- Sys.time()
  
  cat('A dataset_list containing', result_length, 'objects. \n')
  
  cat(paste0('Accessed from ', 
             format(as.POSIXct(now, origin=Sys.time()-as.numeric(Sys.time())), "%Y-%m-%d %H:%M"),
             'h to ',
             format(as.POSIXct(after, origin=Sys.time()-as.numeric(Sys.time())), "%Y-%m-%d %H:%M"),
             'h. \n',
             'Datasets:\n'))
  
  return(output)
}


#' @title Get Dataset Numeric
#' @import lubridate
#' @importFrom methods new
#' @param datasetid Use a single number to extract site information
#' @param ... arguments in ellipse form
#' @export
get_datasets.numeric <- function(datasetid, ...) {
  

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
  
  baseURL <- paste0('data/datasets/', dataset)
  
  result <- parseURL(baseURL)
  
  output <- parse_dataset(result)
  
  return(output)
}

