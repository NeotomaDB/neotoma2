#' @title Read the GPS location of a collection unit
#' @description The `downloads` endpoint returns the GPS location of a
#' collection unit as a list with `latitude`, `longitude`, `gpsaltitude` and
#' `gpserror`, all `null` when no GPS fix was recorded. When the coordinates are
#' present they are turned into a one-row `sf` point (WGS84), so
#' `sf::st_coordinates(cu@gpslocation)` returns `X` (longitude) and `Y`
#' (latitude). `gpsaltitude` and `gpserror` are kept as attribute columns.
#' @param gps The `gpslocation` element of a collection unit, or an `sf`
#' object, or `NULL`.
#' @returns A one-row `sf` object, or an empty `sf` object if there is no
#' usable coordinate pair.
#' @noRd
read_gpslocation <- function(gps) {
  if (inherits(gps, "sf")) {
    return(use_na(gps, "sf"))
  }
  num <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) == 1) v else NA_real_
  }
  lat <- num(gps$latitude)
  long <- num(gps$longitude)
  if (is.na(lat) || is.na(long)) {
    return(sf::st_as_sf(sf::st_sfc()))
  }
  sf::st_as_sf(
    data.frame(
      gpsaltitude = num(gps$gpsaltitude),
      gpserror = num(gps$gpserror),
      long = long,
      lat = lat
    ),
    coords = c("long", "lat"),
    crs = 4326
  )
}

#' @title Build a collection unit from the API response
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom assertthat assert_that
#' @description
#' Helper function to build a collection unit from the API JSON response. This
#' is the one place where the API's collection unit field names are mapped onto
#' the slots of a `collunit`.
#'
#' Two of those fields are spelled differently depending on the endpoint that
#' answered, so `pick()` is used to take whichever spelling arrived:
#' the unit type is `collectionunittype` from `sites`, `unittype` from
#' `datasets` and `collunittype` from `downloads`.
#' @param x The structured JSON from a Neotoma API v2.0 response that
#'   returns a collection unit in any form. `datasets`, `chronologies` and
#'   `speleothems`, if present, are already built objects.
#' @returns A simple `collunit` object
#' @noRd
build_collunits <- function(x) {
  assert_that(is.list(x), msg = "Parsed object must be a list.")
  # A collection unit coming from the API must carry a real collectionunitid.
  # When it does not, the element is malformed/empty, so we drop it rather than
  # fabricate a phantom collunit with a random id (see set_collunit()). The
  # caller null-filters before building the `collunits` container.
  if (is.null(x$collectionunitid)) {
    return(NULL)
  }
  cu <- set_collunit(
    collectionunitid = use_na(x$collectionunitid, "int"),
    collunittype = use_na(pick(
      x, "collectionunittype", "unittype",
      "collunittype"
    ), "char"),
    handle = use_na(x$handle, "char"),
    collectiondevice = use_na(x$collectiondevice, "char"),
    collectionunitname = use_na(pick(
      x, "collectionunitname",
      "collectionunit"
    ), "char"),
    waterdepth = use_na(x$waterdepth, "int"),
    colldate = use_na(as.Date(x$colldate), "date"),
    depositionalenvironment = use_na(x$depositionalenvironment, "char"),
    location = use_na(x$location, "char"),
    gpslocation = read_gpslocation(x$gpslocation),
    collectors = lapply(x$collectors, function(y) y$contactname),
    notes = use_na(x$notes, "char"),
    datasets = testNull(x$datasets, NULL),
    defaultchronology = use_na(x$defaultchronology, "int"),
    chronologies = testNull(x$chronologies, NULL),
    speleothems = testNull(x$speleothems, NULL)
  )
  cu
}
