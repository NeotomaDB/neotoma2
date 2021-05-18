#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @param siteid site unique identificator if available 
#' @param sitename actual site name
#' @param coordinates coordinates to create sf object
#' @param description description of site
#' @param notes additional information of the site
#' @param collunits collection units in the site
#' @param altitude altitude/elevation of the site
#' @export
#' @examples
#' my_site <- set_site(sitename="My Lake", coordinates = c(10, -30),
#'                     description = "my lake", altitude = 30)

set_site <- function(siteid, sitename= NA_character_, 
                             coordinates = c(),
                             description = NA_character_, 
                             notes = NA_character_, collunits = new("collunits"), altitude = NA_integer_){
  x <- new("site")
  #x@siteid <- siteid
  x@sitename <- sitename
  xy = data.frame(x=coordinates[1], y=coordinates[2])
  xy = st_as_sf(xy, coords=c("x","y"))
  x@location <- xy
  x@description <- description
  x@notes <- notes
  x@collunits <- collunits
  x@altitude <- altitude
 
  
  return(x)
}