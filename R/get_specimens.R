#' @title get_specimens
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import geojsonsf
#' @importFrom methods new
#' @description
#' Information for Specimens
#' @param x Use a single datasetid
#' @return The function returns a specimens list
#' @examples \dontrun{
#' # To find all datasets with a min altitude of 12 and a max altitude of 25:
#' my_specimens <- get_specimens(19832)
#' }
#' @export
get_specimens <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_specimens", x)
  } else {
    UseMethod("get_specimens", NA)
  }
}


#' @title Get Dataset Numeric
#' @param x Use a single number to extract site information
#' @param ... Additional parameters to get_specimens
#' @export
get_specimens.numeric <- function(x) {
  use_na <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }
  
  
  base_url <- paste0("data/specimens/", x)
  result <- neotoma2::parseURL(base_url)
  #print(result)
  result_length <- length(result[2]$data)
  
  specimens <- map(result$data, build_specimen)
  print("specimens")
  print(specimens)
  specimens <- new("specimens", specimens = specimens)
}
