#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @param siteid The unique site id for a site.  If this site is new to Neotoma then leave the ID as NA (the default).
#' @param sitename Actual site name as a character string.
#' @param geography An \code{sf} object representing the site location, either as a polygon or point.
#' @param altitude altitude/elevation of the site.
#' @param geopolitical The geopolitical unit in which the site is located.
#' @param area The area of the site or depositional basin in *ha*.  Can be calculated from the polygon.
#' @param description A character description of site.
#' @param notes additional information of the site
#' @param collunits Collection units in the site
#' @export
#' @examples
#' \dontrun{
#' # Create a site called "My Lake", to 
#' x = st_as_sf(st_sfc(st_point(c(5,5))))
#' my_site <- set_site(sitename = "My Lake",
#'                     geography = x,
#'                     description = "my lake", 
#'                     altitude = 30)
#' }

set_site <- function(siteid = NA_integer_,
                     sitename= NA_character_,
                     geography = st_as_sf(st_sfc()),
                     description = NA_character_,
                     notes = NA_character_,
                     collunits = new("collunits"),
                     altitude = NA_integer_) {

  x <- new("site")
  x@siteid <- siteid
  x@sitename <- sitename
  x@geography <- geography
  x@description <- description
  x@notes <- notes
  x@collunits <- collunits
  x@altitude <- altitude

  # TODO : change coordinates to sf_sfc or as is so user can define

  return(x)
}