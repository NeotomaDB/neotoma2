#' @title to_json
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import sf
#' @import jsonify
#' @description
#' Convert a Site object into a json file for API management
#' @param x sites R object to be converted
#' @return The function returns a json object
#' @examples \dontrun{
#' # To find all sites that contain the string "Alex%"
#' alex.sites <- get_sites(sitename="Alex%")
#'
#' Convert the object to json
#' to_json(alex.sites)
#' @export
to_json.sites <- function(x = NA, ...) {
  
  output <- purrr::map(x@sites, function(y){
    jsonify::to_json(list(siteid = y@siteid, 
                          sitename = y@sitename,
                          sitedescription = y@description,
                          geography = '{\"type\":\"Point\",\"crs\":"}',
                          altitude = y@altitude,
                          collectionunits = purrr::map(y@collunits@collunits, function(z){
                            jsonify::to_json(list(
                              handle = z@handle,
                              datasets = purrr::map(z@datasets@datasets, function(d){
                                jsonify::to_json(list(
                                  datasetid = d@datasetid,
                                  datasettype = d@datasettype
                                ), unbox=TRUE)
                              }),
                              collectionunit = z@collectionunitname,
                              collectionunitid = z@collectionunitid
                            ), unbox = TRUE
                            )
                          })
    ), unbox = TRUE
    )
    })

  return(output)
  
}

