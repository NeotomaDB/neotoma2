#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @param siteid site unique identificator if available 
#' @param sitename actual site name
#' @param coordinates sf object
#' @param description description of site
#' @param notes additional information of the site
#' @param collunits collection units in the site
#' @param altitude altitude/elevation of the site
#' @export
#' @examples
#' \dontrun{
#' x = st_as_sf(st_sfc(st_point(c(5,5))))
#' my_site <- set_site(sitename="My Lake", coordinates = x,
#'                     description = "my lake", altitude = 30)
#'                     }

set_site <- function(sitename= NA_character_, 
                             coordinates = st_as_sf(st_sfc()),
                             description = NA_character_, 
                             notes = NA_character_, collunits = new("collunits"), altitude = NA_integer_){
  x <- new("site")
  x@sitename <- sitename
  x@location <- coordinates
  x@description <- description
  x@notes <- notes
  x@collunits <- collunits
  x@altitude <- altitude
 
  # TODO : change coordinates to sf_sfc or as is so user can define
  
  return(x)
}