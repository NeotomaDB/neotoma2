#' @title Build a `site` from the Neotoma API response.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom assertthat assert_that
#' @importFrom sf st_read
#' @description
#' Helper function to build a site from the API JSON response. This is the one
#' place where the API's site field names are mapped onto the slots of a `site`.
#'
#' The site notes are spelled `sitenotes` by the `sites` and `datasets`
#' endpoints but `notes` by `downloads`, so `pick()` takes whichever arrived.
#' @param x A list returned from the Neotoma API `data` section. `collunits`,
#' if present, is an already built object.
#' @returns A simple `site` object
#' @noRd
build_site <- function(x) {
  assert_that(is.list(x), msg = "Parsed object must be a list.")
  # A site coming from the API must carry a real siteid. When it does not, the
  # element is malformed/empty, so we drop it rather than fabricate a phantom
  # site with a random id (see set_site()). The caller null-filters before
  # building the `sites` container. Note the siteid can arrive either absent
  # (NULL) or present-but-NA/empty; both must be skipped.
  if (is.null(x$siteid) || all(is.na(x$siteid))) {
    return(NULL)
  }
  geography <- use_na(read_geography(x$geography), "sf")
  site <- set_site(siteid = use_na(x$siteid, "int"),
                   sitename = use_na(x$sitename, "char"),
                   geography = geography,
                   altitude = use_na(x$altitude, "int"),
                   geopolitical = use_na(x$geopolitical, "list"),
                   area = use_na(x$area, "int"),
                   notes = use_na(pick(x, "sitenotes", "notes"), "char"),
                   description = use_na(x$sitedescription, "char"),
                   collunits = x$collunits)
  return(site)
}

#' @title read_geography
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom sf st_read
#' @description An internal helper that turns the GeoJSON string the API
#' reports for a site into an `sf` object. A site with no coordinates returns
#' `NULL`, which `use_na()` then turns into an empty geometry.
#' @param geography The `geography` field of an API site response.
#' @returns An `sf` object, or `NULL` if there is nothing to read.
#' @noRd
read_geography <- function(geography) {
  if (is.null(geography) || all(is.na(geography))) {
    return(NULL)
  }
  tryCatch(st_read(geography, quiet = TRUE), error = function(e) NULL)
}
