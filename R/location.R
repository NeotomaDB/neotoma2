#' @title parse_location
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import geojsonsf
#' @importFrom methods new
#' @description
#' Retrieve location from datasdets WT, geojson, bounding box
#' @param x location object

parse_location <- function(x) {
  if (is.numeric(x)) {
    # We're getting a numeric vector of coordinates:
    coords <- x
    my_bbox <- sf::st_bbox(c(xmin = coords[1], xmax = coords[2],
                             ymax = coords[3], ymin = coords[4]),
                           crs = st_crs(4326))
    
    if (any(sapply(my_bbox, is.na))) {
      stop("Numeric coordinates need to be an array of 4 units, c(xmin, xmax, ymax, ymin)")
    }
    
    my_bbox <- sf::st_as_sfc(my_bbox)
    
    new_geojson <- geojsonsf::sfc_geojson(my_bbox)
    loc <- new_geojson[1]
    
    
    return(loc[1])
    
  } else if (class(x)[1] == "sf") {
    loc <- geojsonsf::sf_geojson(x)
    
    return(loc)
    
  } else if ("sfg" %in% class(x)) {
    geo <- sf::st_sfc(x)
    loc <- geojsonsf::sfc_geojson(geo)
    
    return(loc)
    
  } else if (is.character(x)) {
    loc <- geojsonsf::geojson_sf(x)
    loc <- parse_location(loc)
    return(loc)
    
  }

}