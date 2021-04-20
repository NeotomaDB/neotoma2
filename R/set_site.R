#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @export
set_site <- function(x = NA, ...) {
  UseMethod('set_site')
}

#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @param siteid helps 
#' @param sitename helps
#' @export
set_site.default <- function(siteid, sitename= NA_character_, 
                             location = st_sf(st_sfc()),
                             description = NA_character_, 
                             notes = NA_character_, collunits = new("collunits")){
  x <- new("site")
  x@siteid <- siteid
  x@sitename <- sitename
  x@location <- location
  x@description <- description
  x@notes <- notes
  x@collunits <- collunits
  
  return(x)
}