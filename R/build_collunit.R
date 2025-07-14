#' @title Build a collection unit from the API response
#' @param args The structured JSON from a Neotoma API v2.0 response that
#'   returns a collection unit in any form.
#' @returns An simple `collunit` object
#' @import sf
#' @keywords internal
#' @noRd
build_collunits <- function(...) {
  args <- list(...)
  assertthat::assert_that(is.list(args),
                          msg = "Parsed object must be a list.")
  cu <- set_collunit(
    collectionunitid = use_na(testNull(args$collectionunitid, NA), "int"),
    collunittype = use_na(testNull(args$collunittype, NA), "char"),
    handle = use_na(testNull(args$handle, NA), "char"),
    collectiondevice = use_na(testNull(args$collectiondevice, NA), "char"),
    collectionunitname = use_na(testNull(args$collectionunitname, NA), "char"),
    waterdepth = use_na(testNull(args$waterdepth, NA), "int"),
    colldate = as.Date(testNull(args$colldate, NA)),
    depositionalenvironment = use_na(testNull(args$depositionalenvironment, NA), "char"),
    location = use_na(testNull(args$location, NA), "char"),
    gpslocation = sf::st_as_sf(sf::st_sfc()),
    notes = use_na(testNull(args$notes, NA), "char"),
    datasets = testNull(args$datasets, NULL),
    defaultchronology = use_na(testNull(args$defaultchronology, NA), "int"),
    chronologies = testNull(args$chronologies, NULL)
  )
  return(cu)
}