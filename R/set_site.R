#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @importFrom methods slot<-
#' @param x Object to be set as a site
#' @param siteid The unique site id for a site.  If this site is new to Neotoma then leave the ID as NA (the default).
#' @param sitename Actual site name as a character string.
#' @param geography An \code{sf} object representing the site location, either as a polygon or point.
#' @param altitude altitude/elevation of the site.
#' @param geopolitical The geopolitical unit in which the site is located.
#' @param area The area of the site or depositional basin in *ha*.  Can be calculated from the polygon.
#' @param description A character description of site.
#' @param notes additional information of the site
#' @param collunits Collection units in the site
#' @returns `site` object
#' @export
#' @examples {
#' # Create a site called "My Lake", to
#' x = sf::st_as_sf(sf::st_sfc(sf::st_point(c(5,5))))
#' my_site <- set_site(sitename = "My Lake",
#'                     geography = x,
#'                     description = "my lake",
#'                     altitude = 30)
#' }

set_site <- function(x = NA,
                     siteid = NA_integer_,
                     sitename = NA_character_,
                     geography = st_as_sf(st_sfc()),
                     altitude = NA_integer_,
                     geopolitical = list(),
                     area = NA_integer_,
                     notes = NA_character_,
                     description = NA_character_,
                     collunits = new("collunits")) {
  
  function_call <- match.call()
  
  if (suppressWarnings(is.na(x))) {
    x <- new("site")
    if (is.na(siteid)) {
      x@siteid <- uuid::UUIDgenerate()
    } else {
      x@siteid <- siteid
    }
    x@sitename <- sitename
    x@geography <- geography
    x@altitude <- altitude
    x@geopolitical <- geopolitical
    x@notes <- notes
    x@description <- description
    x@collunits <- collunits
  } else {
    if (is(x, "site")) {
      if(length(function_call)>2){
        for (i in 3:length(function_call)) {
          slot(x, names(function_call)[[i]]) <- eval(function_call[[i]])
        }
        return(x)
      } else {
        return(x)
      }
    } else {
      stop("`x` must be a site object if it is supplied.")
    }
  }
  return(x)
}