#' @title Build a collection unit from the API response
#' @param x The structured JSON from a Neotoma API v2.0 response that
#'   returns a collection unit in any form.
#' @export
#' @returns An simple `collunit` object
#' @import sf
build_collunits <- function(x) {
  if (length(x$datasets) == 0) {
    # Downloads call
    call_ds <- x$dataset
    datasets <- build_dataset(call_ds)
    datasets <- new("datasets", datasets = list(datasets))

    chronologies <- purrr::map(x$chronologies, build_chron)
    chronologies <- new("chronologies", chronologies = chronologies)
  } else {
    # Sites call
    call_ds <- x$datasets
    datasets <- purrr::map(x$datasets, build_dataset)
    datasets <- new("datasets", datasets = datasets)
    chron <- purrr::map(x$chronologies, build_chron)
    chronologies <- new("chronologies", chronologies = chron)
  }

  if (length(x$handle) == 0) {
    x$handle <- NA
  }

  new_collunits <- new("collunit",
    collunittype = use_na(testNull(x$collectionunittype,
      NA), "char"),
    handle = use_na(testNull(x$handle, NA), "char"),
    collectionunitid = use_na(testNull(x$collectionunitid,
      NA), "int"),
    collectiondevice = use_na(testNull(x$collectiondevice,
      NA), "char"),
    collectionunitname = use_na(testNull(x$collectionunitname,
      NA), "char"),
    waterdepth = use_na(testNull(x$waterdepth, NA), "int"),
    colldate = as.Date(testNull(x$colldate, NA)),
    depositionalenvironment =
      use_na(testNull(x$depositionalenvironment, NA),
      "char"),
    location = use_na(testNull(x$location, NA), "char"),
    gpslocation = sf::st_as_sf(sf::st_sfc()),
    notes = use_na(testNull(x$notes, NA), "char"),
    datasets = datasets,
    defaultchronology = use_na(testNull(x$defaultchronology,
      NA), "int"),
    chronologies = chronologies)
  return(new_collunits)
}
