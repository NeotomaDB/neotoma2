#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Information for Fossil Datasets
#' Look for the whole data details using only a dataset ID
#' or for multiple metadata.
#' Displays a site table with the following columns: siteid,
#' sitename, lat, long, and elev.
#' The function takes parameters defined by the user and
#' returns a sites object
#' with more detailed information regarding datasets and samples.
#' The user may define all or none of the possible fields.
#' The function contains data checks for each defined parameter.
#' @param x Use a single number to extract site information
#' @param ... accepted arguments: sites, datasets
#' @return The function returns either a single item of class
#' \code{"try-error"} describing the reason for failure
#' (either mis-defined parameters or an error from the Neotoma API),
#' or a table of sites, with rows corresponding to the number of
#' individual sites and datasets returned by the Neotoma API.
#' Each "site" object contains 6 parameters that can be accessed as well:
#' \item{ \code{siteid} }{site ID number}
#' \item{ \code{sitename} }{site's name}
#' \item{ \code{location} }{sf object that describes site's location}
#' \item{ \code{description} }{}
#' \item{ \code{collunits} }{limited information on collunits}
#' Each "collection unit" embedded in the "sites" object contains
#' 6 parameters that can be accessed as well:
#' \item{ \code{collunitid}}{collection unit ID number}
#' \item{ \code{handle} }{collection unit's handle}
#' \item{ \code{collunitname} }{collection unit's name}
#' \item{ \code{colldate} }{date in collection unit}
#' \item{ \code{substrate} }{substrate}
#' \item{ \code{location} }{sf object that describes site's location}
#' \item{ \code{datasets} }{detailed information regarding dataset}
#' Each "dataset" nested in the "collection unit" contains the
#' following detail of information:
#' \item{ \code{datasetid} }{dataset ID number}
#' \item{ \code{datasetname} }{site's name}
#' \item{ \code{datasettype} }{type of data found}
#' \item{ \code{location} }{sf object that describes site's location}
#' \item{ \code{notes} }{notes on the dataset}
#' \item{ \code{taxa table} }{taxa table}
#' \item{ \code{pi list} }{P.I. info}
#' \item{ \code{analyst} }{analyst info}
#' \item{ \code{metadata} }{dataset metadata}
#' @examples \dontrun{
#' To find the downloads object of dataset 24:
#' downloads24 <- get_downloads(24)
#'
#' To find all downloads in Brazil
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' brazil_datasets <- get_datasets(loc = brazil[1])
#' brazil_downloads <- get_downloads(brazil_datasets)
#' }
#' @export
get_downloads <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_downloads", x)
  }
}

parse_download <- function(result) { # nolint
  
  data <- result$data %>%
    cleanNULL()
  
  newSites <- map(data, function(x) {

    if (is.na(x$site$geography)) {
      geography <- st_as_sf(st_sfc())
    } else {
      #geography <- try(sf::geojson_sf(x$site$geography))
      geography <- try(sf::st_read(x$site$geography, quiet = TRUE))
      
      if ('try-error' %in% class(geography)) {
        stop('Invalid geoJSON passed from the API. \nCheck that:\n', x$site$geography, 
             '\n is valid geoJSON using a service like http://geojson.io/. If the geojson ',
             'is invalid, contact a Neotoma administrator.')
      }
    }
    
    
    collunits <- new('collunits', 
                     collunits = list(build_collunit(x$site$collectionunit)))
    
      
    set_site(sitename = use_na(x$site$sitename, "char"),
             siteid   = use_na(x$site$siteid, "int"),
             geography = geography,
             altitude = use_na(x$site$altitude, "int"),
             description = use_na(x$site$sitedescription, "char"),
             notes = use_na(x$site$notes, "char"),
             collunits = collunits)
  })
  
  sites <- new('sites', sites = newSites)
  
  return(sites)
}

#' @title get_downloads
#' @param x Use a single number to extract site information
#' @param ... arguments in ellipse form
#' @export
get_downloads.numeric <- function(x, ...) {

  use_na <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }

  cl <- as.list(match.call())

  possible_arguments <- c("offset", "all_data", "x")

  cl[[1]] <- NULL

  for (name in names(cl)) {
    if (!(name %in% possible_arguments)) {
      message(paste0(name, " is not an allowed argument.
      Choose from the allowed arguments: sitename, altmax, altmin, loc"))
    }
  }

  if (length(x) > 0) {
    dataset <- paste0(x, collapse = ",")
  }

  base_url <- paste0("data/downloads/", dataset)
  result <- parseURL(base_url) # nolint

  output <- parse_download(result)

  return(output)
}

#' @title get_downloads sites
#' @param x sites object
#' @param ... arguments in ellipse form
#' @export
get_downloads.sites <- function(x, ...) {

  use_na <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }

  cl <- as.list(match.call())

  possible_arguments <- c("x", "offset", "all_data", "datasetid")

  cl[[1]] <- NULL

  for (name in names(cl)) {
    if (!(name %in% possible_arguments)) {
      message(paste0(name, " is not an allowed argument.
      Choose from the allowed arguments: sitename, altmax, altmin, loc"))
    }
    }

  dataset_list <- c()
  for (i in seq_len(length(x))) {
    collunits_call <- x@sites[[i]]@collunits@collunits
    for (j in seq_len(length(collunits_call))) {
      for (k in seq_len(length(collunits_call[[j]]@datasets@datasets))) {
        datasetid <- collunits_call[[j]]@datasets@datasets[[k]]@datasetid
        dataset_list <- c(dataset_list, datasetid)
      }
    }
  }

  output <- get_downloads(dataset_list)

  return(output)
}