#' @title Build a collection unit from the API response
#' @param x The structured JSON from a Neotoma API v2.0 response that
#'   returns a collection unit in any form.
#' @export
#' @return An object of class \code{collunit}
#' @import digest
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
  
  new_collunit <- set_collunit(
    x = NA,
    collectionunitid = use_na(testNull(x$collectionunitid, NA), "int"),
    notes = use_na(testNull(x$notes, NA), "char"),
    handle = use_na(testNull(x$handle, NA), "char"),
    colldate = as.Date(testNull(x$colldate, NA)),
    location = use_na(testNull(x$location, NA), "char"),
    waterdepth = use_na(testNull(x$waterdepth, NA), "int"),
    gpslocation = sf::st_as_sf(sf::st_sfc()),
    collunittype = use_na(testNull(x$collectionunittype, NA), "char"),
    collectiondevice = use_na(testNull(x$collectiondevice, NA), "char"),
    collectionunitname = use_na(testNull(x$collectionunitname, NA), "char"),
    depositionalenvironment = use_na(testNull(x$depositionalenvironment, NA), 
                                     "char"),
    datasets = datasets,
    chronologies = chronologies,
    defaultchronology = use_na(testNull(x$defaultchronology, NA), "int"))
  
  attributes(new_collunit)$hash <- digest(as.data.frame(new_collunit))
  
  return(new_collunit)
}
