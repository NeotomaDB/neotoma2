#' @title get_datasets
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import geojsonsf
#' @importFrom methods new
#' @description
#' Information for Fossil Datasets
#' Look for a dataset details using only a dataset ID or for multiple metadata.
#' Displays a table with the following columns:
#' siteid, sitename, lat, long, and elev.
#' The function takes parameters defined by the user and returns a sites object
#' with more detailed information regarding datasets.
#' The user may define all or none of the possible fields.
#' The function contains data checks for each defined parameter.
#' @param x Use a single number to extract site information
#' @param ... accepted arguments: sites_object, contactid, datasettype,
#' altmin, altmax, loc, ageyoung, ageold, ageof
#' @return The function returns either a single item of class
#' \code{"try-error"} describing the reason for failure
#' (either mis-defined parameters or an error from the Neotoma API),
#' or a table of sites, with rows corresponding to the number of individual
#' sites and datasets returned by the Neotoma API.
#' Each "site" object contains 6 parameters that can be accessed as well:
#' \item{ \code{siteid} }{site ID number}
#' \item{ \code{sitename} }{site"s name}
#' \item{ \code{location} }{sf object that describes site"s location}
#' \item{ \code{description} }{}
#' \item{ \code{collunits} }{limited information on collunits}
#' Each "collection unit" embedded in the "sites" object contains 6 parameters
#' that can be accessed as well:
#' \item{ \code{collunitid}}{collection unit ID number}
#' \item{ \code{handle} }{collection unit"s handle}
#' \item{ \code{collunitname} }{collection unit"s name}
#' \item{ \code{colldate} }{date in collection unit}
#' \item{ \code{substrate} }{substrate}
#' \item{ \code{location} }{sf object that describes site"s location}
#' \item{ \code{datasets} }{detailed information regarding dataset}
#' Each "dataset" nested in the "collection unit" contains the following detail
#' of information:
#' \item{ \code{datasetid} }{dataset ID number}
#' \item{ \code{datasetname} }{site"s name}
#' \item{ \code{datasettype} }{type of data found}
#' \item{ \code{location} }{sf object that describes site"s location}
#' \item{ \code{notes} }{notes on the dataset}
#' \item{ \code{taxa table} }{taxa table}
#' \item{ \code{pi list} }{P.I. info}
#' \item{ \code{analyst} }{analyst info}
#' \item{ \code{metadata} }{dataset metadata}
#' @examples \dontrun{
#' # To find all datasets with a min altitude of 12 and a max altitude of 25:
#' sites_12to25 <- get_datasets(altmin=12, altmax=25)
#'
#' # To find all datasets in Brazil
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' brazil_datasets <- get_datasets(loc = brazil[1])
#' }
#' @export
get_datasets <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_datasets", x)
  } else {
    UseMethod("get_datasets", NA)
  }
}

parse_dataset <- function(result) { # nolint

  fix_null <- function(x) {
    for (i in seq_len(length(x))) {
      if (is.null(x[[i]])) {
        x[[i]] <- NA
      } else {
        if (class(x[[i]]) == "list") {
          x[[i]] <- fix_null(x[[i]])
        }
      }
    }
    return(x)
  }

  data <- result$data %>%
    fix_null()

  # With a large dataset this seems to take some time, but it's not too bad.
  newSites <- map(data, function(x) {
    if (is.null(x$sites)) {
      call <- x$site
    } else {
      call <- x$sites$site
    }
    if (is.na(call$geography)) {
      geography <- st_as_sf(st_sfc())
    } else {
      geography <- try(sf::st_read(call$geography, quiet = TRUE))

      if ("try-error" %in% class(geography)) {
        stop("Invalid geoJSON passed from the API. \nCheck that:\n",
          call$geography,
          "\n is valid geoJSON using a service like ",
          "http://geojson.io/. If the geojson ",
          "is invalid, contact a Neotoma administrator.")
      }
    }

    if (length(x$sites$datasets) == 0) {
      datasets_ <- map(x$site$datasets, build_dataset)
      datasets_ <- new("datasets", datasets = datasets_)
    } else {
      datasets_ <- map(x$sites$datasets, build_dataset)
      datasets_ <- new("datasets", datasets = datasets_)
    }
    collunits <- new("collunits",
                     collunits = list())

    # Collunits
    # TODO: Implement build collunit
    new_collunit <- new("collunit",
                        collectionunitid = call$collectionunitid,
                        #colldate = as.Date(character(0)),
                        colldate = as.Date(testNull(call$colldate, NA)),
                        handle = call$handle,
                        datasets = datasets_,
                        chronologies = new("chronologies",
                                           chronologies = list()))

    collunits <- new("collunits", collunits = list(new_collunit))

    # Site
    # API error does not allow for build site usage yet.
    set_site(sitename = use_na(call$sitename, "char"),
             siteid   = use_na(call$siteid, "int"),
             geography = geography,
             altitude = use_na(call$altitude, "int"),
             description = use_na(call$sitedescription, "char"),
             notes = use_na(call$sitenotes, "char"),
             collunits = collunits)
  })

  sites <- new("sites", sites = newSites)

  ## Patch to remove repeated sites
  ## This is the chunk that's taking the most time.
  sites <- clean(sites)

  return(sites)

}

#' @title Get Dataset Default
#' @param x Use a single number to extract site information
#' @param ... contactid, datasettype,
#' altmin, altmax, loc, ageyoung, ageold, ageof
#' @examples \dontrun{
#' # To find all datasets with a min altitude of 12 and a max altitude of 25:
#' sites_12to25 <- get_datasets(altmin=12, altmax=25)
#'
#' # To find all datasets in Brazil
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' brazil_datasets <- get_datasets(loc = brazil[1])
#' }
#' @export
get_datasets.default <- function(x, ...) { # nolint

  cl <- as.list(match.call())

  possible_arguments <- c("contactid", "datasettype", "altmin", "altmax", "loc",
                          "ageyoung", "ageold", "ageof", "limit", "offset",
                          "all_data", "sites_o")

  cl[[1]] <- NULL

  cl <- lapply(cl, eval, envir = parent.frame())
  all_data <- ifelse(is.null(cl$all_data), FALSE, TRUE)
  error_check <- check_args(cl) # nolint
  if (error_check[[2]]$flag == 1) {
    stop(paste0(unlist(error_check[[2]]$message), collapse = "\n  "))
  } else {
    cl <- error_check[[1]]
  }

  # Location geojson / coords array
  if ("loc" %in% names(cl)) {
    loc <- parse_location(cl$loc)
    base_url <- paste0("data/datasets?loc=", loc)

    for (name in names(cl)) {
      if (!(name == "loc")) {
        if (!(name == "all_data")) {
          base_url <- paste0(base_url, "&", name, "=", paste0(cl[name]))
        }
      }
    }

    # loc and all_data present
    if ("all_data" %in% names(cl)){
      result <- parseURL(base_url, all_data = cl$all_data) %>%
        cleanNULL()
    } else {
      result <- parseURL(base_url) %>%
        cleanNULL()
    }
  } else {
    base_url <- paste0("data/datasets")
    result <- parseURL(base_url, ...) %>%
      cleanNULL()
  }

  if (is.null(result$data[1][[1]]) || is.null(result[1][[1]])) {
    return(NULL)
  } else {
    output <- parse_dataset(result)
    return(output)
  }

}

#' @title Get Dataset Numeric
#' @param x Use a single number to extract site information
#' @param ... Additional parameters to get_datasets
#' @examples \dontrun{
#' allds <- get_datasets(1:29)
#' plotLeaflet(allds)
#' }
#' @export
get_datasets.numeric <- function(x, ...) {
  use_na <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }

  if (length(x) > 0) {
    dataset <- paste0(x, collapse = ",")
  }

  base_url <- paste0("data/datasets/", dataset)
  result <- neotoma2::parseURL(base_url, ...)
  result_length <- length(result[2]$data)

  if (result_length > 0) {
    output <- parse_dataset(result)
    return(output)
  } else {
    return(NULL)
  }
}

#' @title Get Dataset from a \code{sites} object.
#' @param x An object of class \code{sites}.
#' @param ... additional arguments accepted by \code{get_datasets()}
#' @examples \dontrun{
#' random_sites <- get_sites(1:100)
#' allds <- get_datasets(random_sites)
#' plotLeaflet(allds)
#' }
#' @export
get_datasets.sites <- function(x, ...) {
  # List of datasets ids
  dataset_list <- getids(x)$datasetid
  dataset_list <- as.numeric(unlist(dataset_list))

  output <- get_datasets(dataset_list, all_data = TRUE)

  return(output)
}
