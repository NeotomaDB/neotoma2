#' @md
#' @title Build a `site` from the Neotoma API response.
#' @param x A list returned from the Neotoma API `data` section.
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @import sf
#' @export
#' @returns A simple `site` object

build_sites <- function(x) {
  assertthat::assert_that(is.list(x),
                          msg = "Parsed object must be a list.")
  new_sites <- purrr::map(x, function(x) {
    if (length(x$geography) == 0) {
      x$geography <- NA
    }
    if (is.na(x$geography)) {
      geography <- st_as_sf(st_sfc())
    } else if (!(is.na(x$geography))) {
      geography <- sf::st_read(x$geography, quiet = TRUE)
    } else if (!(is.na(x$site$geography))) {
      geography <- try(sf::st_read(x$site$geography, quiet = TRUE))
      if ("try-error" %in% class(geography)) {
        stop("Invalid geoJSON passed from the API. \nCheck that:\n",
             x$site$geography,
             "\n is valid geoJSON using a service like http://geojson.io/. ",
             "If the geojson is invalid, contact a Neotoma administrator.")
      } else {
        geography <- st_as_sf(st_sfc())
      }
    }
    
    if (is.null(x$collectionunits)) {
      # Dw call
      cu_call <- x$collectionunit
      collunits <- build_collunits(cu_call)
      collunits <- new("collunits", collunits = list(collunits))
      
    } else {
      # Sites Call
      cu_call <- x$collectionunits
      collunits <- purrr::map(cu_call, build_collunits)
      collunits <- new("collunits", collunits = collunits)
    }
    
    if (is.null(x$geopolitical)) {
      geopolitical <- list()
    } else {
      geopolitical <- list(x$geopolitical)
    }
    
    if (is.na(suppressWarnings(as.numeric(x$siteid)))) {
      df <- as.data.frame(datasets(collunits))
      dsid <- unique(df$datasetid)
      warnsite <- paste0(sprintf("Dataset(s) %s may have been recently removed from the database.",
                              paste0(dsid,collapse = ", ")),
                      " Affected sites/datasets will be removed when you do `get_datasets` or `get_downloads`",
                      sep = "\n")
      warning(warnsite)
    }
    set_site(siteid   = use_na(testNull(x$siteid, NA), "int"),
             sitename = use_na(testNull(x$sitename, NA), "char"),
             geography = geography,
             altitude = use_na(testNull(x$altitude, NA), "int"),
             geopolitical = geopolitical,
             notes = use_na(testNull(x$notes, NA), "char"),
             description = use_na(testNull(x$sitedescription, NA), "char"),
             collunits = collunits)
  }
  )
  
  sites <- new("sites", sites = new_sites)
  return(sites)
}
