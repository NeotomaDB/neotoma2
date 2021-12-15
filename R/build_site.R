#' @title Build a \code{site} from the Neotoma API response.
#' @param x A list returned from the Neotoma API's \code{data} slot.
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @import sf
#' @examples \dontrun{
#' response <- jsonlite::fromJSON('https://api.neotomadb.org/v2.0/data/datasets/100,101',
#'                                flatten=FALSE, simplifyVector=FALSE)
#' response <- cleanNULL(response)
#' newSites <- build_sites(response$data)
#' newSites
#' }

build_sites <- function(x) {
  assertthat::assert_that(is.list(x),
                          msg = "Parsed object must be a list.")

  newSites <- purrr::map(x, function(x) {
    if(!(is.na(x$geography))){
      geography <- sf::st_read(x$geography, quiet = TRUE)
    } else if (!(is.na(x$site$geography))) {
      geography <- try(sf::st_read(x$site$geography, quiet = TRUE))
      if ('try-error' %in% class(geography)) {
        stop('Invalid geoJSON passed from the API. \nCheck that:\n', x$site$geography, 
             '\n is valid geoJSON using a service like http://geojson.io/. If the geojson ',
             'is invalid, contact a Neotoma administrator.')
      } else {
        geography <- st_as_sf(st_sfc())
      }
    }
    
    if(is.null(x$collectionunits)){
      # Dw call
      cu_call <- x$collectionunit
      collunits <- build_collunits(cu_call)
      collunits <- new("collunits", collunits = list(collunits))
      
    }else{
      # Sites Call
      cu_call <- x$collectionunits
      collunits <- purrr::map(cu_call, build_collunits)
      collunits <- new("collunits", collunits = collunits)
    }
    
    set_site(siteid   = use_na(x$siteid, "int"),
             sitename = use_na(x$sitename, "char"),
             geography = geography,
             altitude = use_na(x$altitude, "int"),
             geopolitical = testNull(x$geopolitical, list()), 
             notes = use_na(testNull(x$notes, NA), "char"),
             description = use_na(x$sitedescription, "char"),
             collunits = collunits)
  })
  sites <- new('sites', sites = newSites)
}