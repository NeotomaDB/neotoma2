#' @md
#' @title parseLocation
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @param x location parameter
#' @returns `geojson` object to be used for API
#' @keywords internal
#' @noRd
parseLocation <- function(x) {
  if (is.numeric(x)) {
    assertthat::assert_that(length(na.omit(x)) == 4,
                            msg = "Numeric coordinates need to be an array of 4 units, c(xmin, xmax, ymax, ymin)")
    bbox <- sf::st_bbox(c(xmin = x[1], xmax = x[3],
                          ymax = x[4], ymin = x[2]),
                        crs = sf::st_crs(4326))
    sfc <- sf::st_as_sfc(bbox)
    geojson <- geojsonsf::sfc_geojson(sfc)
    geojson <- jsonlite::fromJSON(geojson, simplifyVector = FALSE)
  } else if (inherits(x, "sf")) {
    geojson <- geojsonsf::sf_geojson(x)
    geojson <- jsonlite::fromJSON(geojson, simplifyVector = FALSE)
  } else if (inherits(x, "sfg")) {
    geojson <- geojsonsf::sfc_geojson(sf::st_sfc(x))
    geojson <- jsonlite::fromJSON(geojson, simplifyVector = FALSE)
  } else if (is.character(x)) {
    geojson <- tryCatch({
      parsed <- geojsonsf::geojson_sf(x)
      if (inherits(parsed, "sf")) {
        gj <- geojsonsf::sf_geojson(parsed)
        jsonlite::fromJSON(gj, simplifyVector = FALSE)
      } else {
        stop("Input string is not valid GeoJSON.")
      }
    }, error = function(e) {
      stop("Error parsing GeoJSON string: ", e$message)
    })
  }
  body <- list(jsonlite::toJSON(geojson, auto_unbox = TRUE))
  return(body)
}