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
  result
  #result_length <- length(result[2]$data)
  
  

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
  print(result)
  output <- parse_download(result)
  
  return(output)
}

