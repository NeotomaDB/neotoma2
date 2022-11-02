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
    my_bbox <- sf::st_bbox(c(xmin = coords[1], xmax = coords[3],
                             ymax = coords[4], ymin = coords[2]),
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
    loc1 <- try(geojsonsf::geojson_sf(x), silent = TRUE)
    if ("try-error" %in% class(loc1)) {
      loc <- sf::st_as_sf(wk::new_wk_wkt(x))
      loc <- parse_location(loc)
    } else {
      loc <- parse_location(loc1)
    }
    return(loc)
  }
}