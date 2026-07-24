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
    if (is_wkt(x)) {
      # WKT is not accepted by the API; parse it locally and continue down the
      # existing sfc -> GeoJSON path so the request body is unchanged in shape.
      sfc <- st_as_sfc(x, crs = 4326)
      geojson <- sfc_geojson(sfc)
      geojson <- fromJSON(geojson, simplifyVector = FALSE)
    } else {
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
  }
  body <- list(toJSON(geojson, auto_unbox = TRUE))
  return(body)
}

#' @title is_wkt
#' @description Detect whether a character `loc` string is Well-Known Text
#' (WKT) rather than GeoJSON. The test is anchored on the start of the
#' (trimmed) string so a GeoJSON object such as `{"type":"Polygon",...}` --
#' which contains the word `POLYGON` -- is never misread as WKT. An optional
#' `SRID=...;` (EWKT) prefix is allowed.
#' @param x A single character string.
#' @returns `TRUE` if `x` looks like a WKT/EWKT geometry, otherwise `FALSE`.
#' @keywords internal
#' @noRd
is_wkt <- function(x) {
  is.character(x) && length(x) == 1L &&
    grepl(paste0("^\\s*(SRID=\\d+;)?\\s*",
                 "(POINT|LINESTRING|POLYGON|MULTIPOINT|",
                 "MULTILINESTRING|MULTIPOLYGON|GEOMETRYCOLLECTION)"),
          x, ignore.case = TRUE)
}
