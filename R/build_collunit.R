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
    collunittype = use_na(pick(x, "collectionunittype", "unittype",
                               "collunittype"), "char"),
    handle = use_na(x$handle, "char"),
    collectiondevice = use_na(x$collectiondevice, "char"),
    collectionunitname = use_na(pick(x, "collectionunitname",
                                     "collectionunit"), "char"),
    waterdepth = use_na(x$waterdepth, "int"),
    colldate = use_na(as.Date(x$colldate), "date"),
    depositionalenvironment = use_na(x$depositionalenvironment, "char"),
    location = use_na(x$location, "char"),
    gpslocation = use_na(x$gpslocation, "sf"),
    notes = use_na(x$notes, "char"),
    datasets = testNull(x$datasets, NULL),
    defaultchronology = use_na(x$defaultchronology, "int"),
    chronologies = testNull(x$chronologies, NULL),
    speleothems = testNull(x$speleothems, NULL)
  )
  return(cu)
}
