#' @title Get datasets - Data Sets Information for Fossil Sites
#' @import gtools
#' @param datasetid integer A collection unit ID
#' @export
get_datasets <- function(datasetid = NA, ...) {
  if(!missing(datasetid)) {
    UseMethod('get_datasets', datasetid)
  } else {
    UseMethod('get_datasets', NA)
  }
}

parse_dataset <- function(result) {
  
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
  cat('This is the length:', result_length, '\n')
  
  dataset_list <- c()
  for(i in 1:result_length) {
    # i-th element result[2]$data[[i]]$
    cat("i: ", i)
    datasetid <- result$data[[i]]$site$datasets[[i]]$datasetid
    datasetname <- as.character(result[2]$data[[i]]$site$sitename)
    datasettype <- as.character(result$data[[i]]$site$datasets[[i]]$datasettype)
    notes <- NA_character_
    
    new_dataset <- new('dataset',
                       datasetid = datasetid,
                       datasetname = datasetname,
                       datasettype = datasettype,      
                       notes = notes)
    
  }
  
  return(new_dataset)
}


#' @title Get Dataset Numeric
#' @import lubridate
#' @importFrom methods new
#' @param datasetid Use a single number to extract site information
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
    sitename <- paste0(datasetid, collapse = ',')
  }
  
  baseURL <- paste0('data/datasets/', datasetid)
  
  result <- parseURL(baseURL)
  
  output <- parse_dataset(result)
  
  return(output)
}

