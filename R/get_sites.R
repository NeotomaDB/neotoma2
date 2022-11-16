#' @title parse_site
#' @description An internal helper function to parse the API result into a site object.
#' @param result A JSON object from the API.
#' @return A neotoma2 site object.
parse_site <- function(result) {
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

  # Function to use once API is in order.
  # API - Site currently does not have any 'site'
  # keys. Might need modification afterwards
  newSites <- build_sites(data)

  return(newSites)
}

#' @title get_sites
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @description
#' Information for Fossil Sites
#' Look for a site details using only a site ID or for multiple sites
#' using possible sitenames, max latitudes or min latitudes.
#' Displays a table with the following columns: siteid, sitename, lat, long,
#' and elev.
#' The function takes parameters defined by the user and returns a list
#' of contact information supplied by the Neotoma Paleoecological Database.
#' The user may define all or none of the possible fields.
#' The function contains data checks for each defined parameter.
#' @param x Use a single number to extract site information
#' @param ... accepted arguments: sitename, altmax, altmin, loc, taxa, 
#' contacts, keyword, gpid
#' @return The function returns either a single item of class \code{"try-error"}
#' describing the reason for failure (either mis-defined parameters or an error
#' from the Neotoma API), or a table of sites, with rows corresponding to the
#' number of individual sites returned by the Neotoma API.
#' Each "site" object contains 6 parameters that can be accessed as well:
#' siteid, sitename, location, altitude, description,
#' limited collection units information.
#' \item{ \code{siteid} }{site ID number}
#' \item{ \code{sitename} }{site"s name}
#' \item{ \code{loc} }{An \code{sf} object that describes site's location}
#' \item{ \code{description} }{}
#' \item{ \code{collunits} }{limited information on collunits}
#' @examples \dontrun{
#' ## Find all sites with a min altitude of 12m and a max altitude of 25m
#' ## By default returns only 25 sites (default limit is 25):
#' sites_12to25 <- get_sites(altmin=12, altmax=25)
#' ## Return all sites, using a minimum altitude of 2500m (returns >500 sites):
#' sites_2500 <- get_sites(altmin=2500, all_data = TRUE)
#'
#' ## To find all sites that contain the string "Alex%"
#' alex_sites <- get_sites(sitename="Alex%")
#'
#' ## To find sites in Brazil (again with default 25 records)
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]
#' ]]}'
#' brazil_sites <- get_sites(loc = brazil[1])
#' }
#' @export
get_sites <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_sites", x)
  } else {
    UseMethod("get_sites", NA)
  }
}

#' @title Get Site Information for Fossil Sites
#' @param ... accepted arguments: siteid, sitename, altmax, altmin, loc, gpid
#' @export
get_sites.default <- function(...) { # nolint
  
  cl <- as.list(match.call())
  possible_args <- c("sitename", "altmax", "altmin", "keyword", "taxa", "gpid")
  possible_args2 <- c("loc", "limit", "offset", "all_data", "contacts", "siteid", "datasettype")
  possible_args <- c(possible_args, possible_args2)
  
  cl[[1]] <- NULL
  
  for (name in names(cl)) {
    if (!(name %in% possible_args)) {
      message(paste0(name, " is not an allowed argument.\
      Choose from the allowed arguments: sitename, altmax, altmin, loc, keyword, contacts, taxa"))
    }
  }
  
  cl <- lapply(cl, eval, envir = parent.frame())
  
  error_check <- check_args(cl) # nolint
  
  if (error_check[[2]]$flag == 1) {
    stop(paste0(unlist(error_check[[2]]$message), collapse = "\n  "))
  } else {
    cl <- error_check[[1]]
  }
  # Location geojson / coords array
  if ("loc" %in% names(cl)) {
    loc <- parse_location(cl$loc)
    base_url <- paste0("data/sites?loc=", loc)

    for (name in names(cl)) {
      if (!(name == "loc")) {
        if (!(name == "all_data")) {
        base_url <- paste0(base_url, "&", name, "=", paste0(cl[name]))
        }
      }
    }
    # loc and all_data
    if ("all_data" %in% names(cl)){
      result <- parseURL(base_url, all_data = cl$all_data) %>%
        cleanNULL()
    }else{
      result <- parseURL(base_url) %>%
        cleanNULL()
    } 
  }else{

    base_url <- paste0("data/sites")
    result <- parseURL(base_url, ...) %>%
      cleanNULL()
  }
  
  
  if (is.null(result$data[1][[1]])) {
    return(NULL)
  } else {
    output <- parse_site(result)
    return(output)
  }
  
}

#' @title Get Site Information for Fossil Sites
#' @param x The numeric site ID from Neotoma
#' @param ... accepted arguments if numeric all_data
#' @export
get_sites.numeric <- function(x, ...) {
  
  if (length(x) > 0) {
    siteids <- paste0(x, collapse = ",")
  }
  
  base_url <- paste0("data/sites/", siteids)
  
  result <- neotoma2::parseURL(base_url)
  
  result_length <- length(result[2]$data)
  
  if (result_length > 0) {
    
    output <- parse_site(result)
    
    return(output)
    
  } else {
    return(NULL)
  }
}
