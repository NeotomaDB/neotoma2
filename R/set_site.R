#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @param siteid helps 
#' @param sitename helps
#' @export
#' @examples
#' my_site <- set_site(sitename="My Lake", coordinates = c(10, -30), description = "my lake", altitude = 30)
#' my_site
#' | siteid  | sitename       |    lat    |   long   | elev   |
#' | :------ |:--------------:|:---------:|:--------:|-------:|
#' |   NA    |  My Lake       |     10    |    -30   |   30   |
#' @md


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