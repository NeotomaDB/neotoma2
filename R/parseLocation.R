#' @title parseLocation
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom geojsonsf geojson_sf sfc_geojson sf_geojson
#' @importFrom sf st_as_sfc st_bbox st_crs st_sfc
#' @importFrom assertthat assert_that
#' @importFrom stats na.omit
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @param x location parameter
#' @returns `geojson` object to be used for API requests.
#' @keywords internal
#' @noRd
parseLocation <- function(x) {
  if (is.numeric(x)) {
    assert_that(length(na.omit(x)) == 4,
                msg = "Numeric coordinates need to be an array of
                       4 units, c(xmin, xmax, ymax, ymin)")
    bbox <- st_bbox(c(xmin = x[1], xmax = x[3],
                      ymax = x[4], ymin = x[2]),
                    crs = st_crs(4326))
    sfc <- st_as_sfc(bbox)
    geojson <- sfc_geojson(sfc)
    geojson <- fromJSON(geojson, simplifyVector = FALSE)
  } else if (inherits(x, "sf")) {
    geojson <- sf_geojson(x)
    geojson <- fromJSON(geojson, simplifyVector = FALSE)
  } else if (inherits(x, "sfg")) {
    geojson <- sfc_geojson(st_sfc(x))
    geojson <- fromJSON(geojson, simplifyVector = FALSE)
  } else if (is.character(x)) {
    geojson <- tryCatch({
      parsed <- geojson_sf(x)
      if (inherits(parsed, "sf")) {
        gj <- sf_geojson(parsed)
        fromJSON(gj, simplifyVector = FALSE)
      } else {
        stop("Input string is not valid GeoJSON.")
      }
    }, error = function(e) {
      stop("Error parsing GeoJSON string: ", e$message)
    })
  }
  body <- list(toJSON(geojson, auto_unbox = TRUE))
  return(body)
}
#add wkt