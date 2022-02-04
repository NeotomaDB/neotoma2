#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @import sf
#' @importFrom methods new
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

set_site <- function(x = NA,
                     siteid = NA_integer_,
                     sitename= NA_character_,
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
    x@siteid <- siteid
    x@sitename <- sitename
    x@geography <- geography
    x@altitude <- altitude
    x@geopolitical <- geopolitical
    x@notes <- notes
    x@description <- description
    x@collunits <- collunits  
  } else {
    if (class(x) == "site") {
      for (i in 3:length(function_call)) {
        slot(x, names(function_call)[[i]]) <- function_call[[i]]
      }
    }
    return(x)
  }
  

  # TODO : change coordinates to sf_sfc or as is so user can define

  return(x)
}