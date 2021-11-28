#' @title Build a collection unit from the API response
#' @param x The structured JSON from a Neotoma API v2.0 response that returns a collection unit in any form.
#' @return An object of class \code{collunit}
build_collunit <- function(x) {
  
  if (all(is.na(x$gpslocation))) {
    location <- st_as_sf(st_sfc())
  } else {
    location <- geojson_sf(x$gpslocation)
  }
  
  new('collunit',
      collectionunitid = use_na(x$collectionunitid, "int"),
      handle = use_na(x$handle, "char"),
      collectiondevice = use_na(x$collectiondevice, "char"),
      collectionunitname = use_na(x$collectionunit, "char"),
      collunittype = use_na(x$collunittype, "char"),
      waterdepth = use_na(x$waterdepth, "int"),
      colldate = as.Date(character(0)),
      depositionalenvironment = use_na(x$depositionalenvironment, "char"),
      location = use_na(x$location, "char"),
      gpslocation = st_sf(st_sfc()),
      notes = use_na(x$notes, "char"),
      datasets = new("datasets"),
      chronologies = new("chronologies"))
}
