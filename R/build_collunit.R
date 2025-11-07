#' @title Build a collection unit from the API response
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom assertthat assert_that
#' @param args The structured JSON from a Neotoma API v2.0 response that
#'   returns a collection unit in any form.
#' @returns An simple `collunit` object
#' @noRd
build_collunits <- function(...) {
  args <- list(...)
  assert_that(is.list(args), msg = "Parsed object must be a list.")
  args <- cleanNULL(args)
  cu <- set_collunit(
    collectionunitid = use_na(args$collectionunitid, "int"),
    collunittype = use_na(args$collunittype, "char"),
    handle = use_na(args$handle, "char"),
    collectiondevice = use_na(args$collectiondevice, "char"),
    collectionunitname = use_na(args$collectionunitname, "char"),
    waterdepth = use_na(args$waterdepth, "int"),
    colldate = use_na(args$colldate, "date"),
    depositionalenvironment = use_na(args$depositionalenvironment, "char"),
    location = use_na(args$location, "char"),
    gpslocation = use_na(args$gpslocation, "sf"),
    notes = use_na(args$notes, "char"),
    datasets = testNull(args$datasets, NULL),
    defaultchronology = use_na(args$defaultchronology, "int"),
    chronologies = testNull(args$chronologies, NULL),
    speleothems = testNull(args$speleothems, NULL)
  )
  return(cu)
}