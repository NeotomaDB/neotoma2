#' @title Build a collection unit from the API response
#' @param x The structured JSON from a Neotoma API v2.0 response that returns a collection unit in any form.
#' @return An object of class \code{collunit}
#' @import sf
build_collunit <- function(x) {
  newCollunits <- purrr::map(x, function(x) {
    
    #datasets <- build_datasets(x$dataset)
    print(length(x$dataset[1]))
    datasets <- purrr::map(x$dataset, build_dataset)
    datasets <- new('datasets', datasets = datasets)
    #chronologies <- build_chron(x$chronologies)
    
    y <- new('collunit',
             collunittype = use_na(testNull(x$collectionunittype, NA), "char"),
             handle = use_na(x$handle, "char"),
             collectionunitid = use_na(testNull(x$collectionunitid, NA), "int"),
             
             collectiondevice = use_na(testNull(x$collectiondevice, NA), "char"),
             collectionunitname = use_na(x$collectionunit, "char"),
             
             waterdepth = use_na(testNull(x$waterdepth, NA), "int"),
             colldate = as.Date(character(0)),
             depositionalenvironment = use_na(testNull(x$depositionalenvironment,NA), "char"),
             location = use_na(testNull(x$location, NA), "char"),
             gpslocation = sf::st_as_sf(sf::st_sfc()),
             notes = use_na(testNull(x$notes,NA), "char"),
             datasets = datasets,
             #datasets = new('datasets', datasets = list()),
             #chronologies = chronologies)
             chronologies = new('chronologies', chronologies = list()))
  })
  
  collunits <- new('collunits', collunits = newCollunits)
  
  return(collunits)
}