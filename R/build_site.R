#' @title Build a \code{site} from the Neotoma API response.
#' @param x A list returned from the Neotoma API's \code{data} slot.
#' @importFrom assertthat assert_that
#' @importFrom purrr map
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
  assertthat::assert_that(all(sapply(x, names) == "site"),
                          msg = "All lists passed to build_sites must contain valid sites.")
  
  newSites <- purrr::map(x, function(x) {
    if (is.na(x$site$geography)) {
      geography <- st_as_sf(st_sfc())
    } else {
      geography <- try(geojson_sf(x$site$geography))
      if ('try-error' %in% class(geography)) {
        stop('Invalid geoJSON passed from the API. \nCheck that:\n', x$site$geography, 
             '\n is valid geoJSON using a service like http://geojson.io/. If the geojson ',
             'is invalid, contact a Neotoma administrator.')
      }
    }
    
    collunits <- try(new('collunits', 
                     collunits = list(build_collunit(x$site$collectionunit))), 
                     silent = TRUE)
    
    if ('try-error' %in% class(collunits)) {
      collunits <- new('collunits', 
          collunits = list())
    }
    
    set_site(siteid   = use_na(x$site$siteid, "int"),
             sitename = use_na(x$site$sitename, "char"),
             geography = geography,
             altitude = use_na(x$site$altitude, "int"),
             geopolitical = x$site$geopolitical, 
             notes = use_na(testNull(x$site$notes, x$site$sitenotes), "char"),
             description = use_na(x$site$sitedescription, "char"),
             collunits = collunits)
  })

  sites <- new('sites', sites = newSites)
}
