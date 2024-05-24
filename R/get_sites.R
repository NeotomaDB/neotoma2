#' @md
#' @title parse_site
#' @description An internal helper function to parse the API result into a site object.
#' @param result A JSON object from the API.
#' @returns A Neotoma2 site object.
parse_site <- function(result) {
  fix_null <- function(x) {
    for (i in seq_len(length(x))) {
      if (is.null(x[[i]])) {
        x[[i]] <- NA
      } else {
        if (is(x[[i]], "list")) {
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
#' @author Socorro Dominguez \email{s.dominguez@ht-data.com}
#' @import gtools
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @param x A numeric vector of unique Neotoma site identifiers.
#' @param ... One of a set of possible query parameters discussed in details.
#' @description
#' The get_sites() function is a wrapper for the Neotoma `sites` API
#' endpoint.
#' The function takes parameters defined by the user and returns a list
#' of site information supplied by the Neotoma Paleoecological Database.
#' The user may define all or none of the possible fields.
#' @param x Use a single integer or vector of integers representing 
#' unique Neotoma site identifiers (siteids) to extract site information.
#' @param ... accepted arguments, see details for more information.
#' @details
#' A `site` object in Neotoma is a physical location at which one or more
#' collection units are located. Each collection unit may have one or more
#' datasets within it, defined by the dataset type. The `get_sites()`
#' function searches for each site within Neotoma that matches the query
#' parameters, and returns them as a `sites` object, a list of `site`
#' objects. The `get_sites()` command wraps the Neotoma API
#' ([api.neotomadb.org](https://api.neotomadb.org)) call for `sites`.
#' The call itself uses a SQL query which accepts any one of the following
#' parameters:
#'  * `siteid`  The unique site ID (integer) in Neotoma. Can be passed as a
#' vector of site IDs.
#'  * `sitename`  The site name, or approximate match using the % wildcard.
#'  * `database`  The constituent database for the record. See `get_table("constituentdatabases")`
#'  * `altmin`  The minimum altitude range for site elevation (in meters).
#'  * `altmax`  The maximum altitude range for site elevation (in meters).
#'  * `datasetid`  The unique dataset ID (integer) in Neotoma. Can be passed
#' as a vector of dataset IDs.
#'  * `datasettype`  Neotoma contains data for a number of datasettypes.
#'  This returns a subset of data types. For a complete list of available 
#'  datasettypes, run `neotoma2::get_table('datasettypes')`
#'  * `doi`  The dataset DOI for a dataset contained within a site. Can be
#' passed as a vector of DOIs.
#'  * `gpid`  The geopolitical name or identifier containing a site. Can be
#' passed as a vector of names.
#'  * `keywords`  Keywords for samples within a set of sites. For example
#' "modern" indicates a sample within the record uses the keyword "modern".
#'  * `contacts`  Contact names or IDs associated with a site.
#'  * `ageyoung`  A minimum spanning age for the record, in years before
#' radiocarbon present (1950).
#'  * `ageold`  A maximum spanning age for the record, in years before
#' radiocarbon present (1950).
#'  * `ageof`  An age which must be contained within the range of sample ages
#' for a site.
#'  * `taxa`  The names of taxa which must be present within samples in a
#' record.
#'  * `all_data`  The API only downloads the first 25 records of the query. 
#'  For the complete records, use `all_data=TRUE`
#' This call will then return a data object that contains site metadata for one
#' or more sites, along with limited metadata describing the collection units
#' and datasets located at that site.
#' @returns The function returns either a single item of class `"try-error"`
#' describing the reason for failure (either misdefined parameters or an error
#' from the Neotoma API), or a table of sites, with rows corresponding to the
#' number of individual sites returned by the Neotoma API.
#' Each "site" object contains 6 parameters that can be accessed as well:
#' siteid, sitename, location, altitude, description,
#' limited collection units information.
#'  * `loc` An `sf` object that describes site's location.
#'  * `description`
#'  * `collunits` limited information on collunits
#' @examples
#' \donttest{
#' ## Find all sites with a min altitude of 12m and a max altitude of 25m
#' ## By default returns only 25 sites (default limit is 25):
#' sites_12to25 <- get_sites(altmin=12, altmax=25)
#' ## Return all sites, using a minimum altitude of 2500m (returns >500 sites):
#' sites_2500 <- get_sites(altmin=2500, all_data = TRUE)
#' ## To find all sites that contain the string "Alex%"
#' alex_sites <- get_sites(sitename="Alex%")
#' ## To find sites in Brazil (again with default 25 records)
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' brazil_sites <- get_sites(loc = brazil[1])
#'
#' # Finding all sites with Liliaceae pollen in 1000 year bins:
#' lilysites <- c()
#' for (i in seq(0, 10000, by = 1000)) {
#'   lily <- get_sites(taxa=c("Liliaceae"),
#'                     ageyoung = i - 500,
#'                     ageold = i + 500,
#'                     all_data = TRUE)
#'   lilysites <- c(lilysites, length(lily))
#' }
#' plot(x = seq(0, 10000, by = 1000), y = lilysites, type = 'b')
#' }
#' @export
get_sites <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_sites", x)
  } else {
    UseMethod("get_sites", NA)
  }
}

#' @title get_sites
#' @author Socorro Dominguez \email{s.dominguez@ht-data.com}
#' @import gtools
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @importFrom utils URLencode
#' @param ... One of a set of possible query parameters discussed in details.
#' @returns The function returns either a single item of class `"try-error"`
#' describing the reason for failure (either misdefined parameters or an error
#' from the Neotoma API), or a table of sites, with rows corresponding to the
#' number of individual sites returned by the Neotoma API.
#' Each "site" object contains 6 parameters that can be accessed as well:
#' siteid, sitename, location, altitude, description,
#' limited collection units information.
#'  * `loc` An `sf` object that describes site's location.
#'  * `collunits` limited information on collunits
#' @export
get_sites.default <- function(...) { # nolint
  oo <- options(scipen = 9999999)
  on.exit(options(oo))
  
  cl <- as.list(match.call())

  cl[[1]] <- NULL

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
    base_url <- paste0("data/sites?loc=", URLencode(loc, reserved = TRUE))
    if(length(base_url)>1){
      stop("Multiple polygons cannot be handled, pass one polygon at a time.")
    }

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
      
      # add warning to revise loc argument
    } else {
      result <- parseURL(base_url) %>%
        cleanNULL()
      # add warning to revise loc argument
    }
  } else {

    base_url <- paste0("data/sites")
    result <- parseURL(base_url, ...) 
    
    result <- result %>%
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
#' @returns The function returns either a single item of class `"try-error"`
#' describing the reason for failure (either misdefined parameters or an error
#' from the Neotoma API), or a table of sites, with rows corresponding to the
#' number of individual sites returned by the Neotoma API.
#' Each "site" object contains 6 parameters that can be accessed as well:
#' siteid, sitename, location, altitude, description,
#' limited collection units information.
#'  * `loc` An `sf` object that describes site's location.
#'  * `collunits` limited information on collunits
#' @examples {
#' ## Find all sites by numeric siteid:
#' sites <- get_sites(seq(1,3))
#' }
#' @export
get_sites.numeric <- function(x, ...) {
  if (length(x) > 0) {
    siteids <- paste0(x, collapse = ",")
  }
  base_url <- paste0("data/sites/", siteids)
  result <- neotoma2::parseURL(base_url, ...)
  result_length <- length(result[2]$data)

  if (result_length > 0) {
    output <- parse_site(result)
    return(output)
  } else {
    return(NULL)
  }
}

#' @title Get Site Information for Fossil Sites from a Set of Sites
#' @param x The numeric site ID from Neotoma
#' @param ... accepted arguments if numeric all_data
#' @examples
#' \donttest{
#' ## Find all sites using a set of prior sites:
#' char_sites <- get_sites(taxa = "charcoal")
#' pollen_coloc <- get_sites(char_sites, datasettype = "pollen")
#' char_coloc <- char_sites %>% filter(siteid %in% getids(pollen_coloc)$siteid)
#' pol_char <- c(pollen_coloc, char_coloc)
#' }
#' @returns The function returns either a single item of class `"try-error"`
#' describing the reason for failure (either misdefined parameters or an error
#' from the Neotoma API), or a table of sites, with rows corresponding to the
#' number of individual sites returned by the Neotoma API.
#' Each "site" object contains 6 parameters that can be accessed as well:
#' siteid, sitename, location, altitude, description,
#' limited collection units information.
#'  * `loc` An `sf` object that describes site's location.
#'  * `collunits` limited information on collunits
#' @export
get_sites.sites <- function(x, ...) {
  
  if (length(x) > 0) {
    ids <- getids(x)
    siteids <- ids$siteid
    #datasetids <- ids$datasetid
    siteids <- siteids %>%
      unique() %>%
      as.numeric() %>%
      na.omit() %>%
      suppressWarnings() %>%
      paste0(., collapse = ",")
  }
 
  base_url <- "data/sites"
  
  ## Fixing all data
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  
  if('all_data' %in% names(cl)){
    all_data = cl$all_data
  }else{
    cl[['all_data']] = TRUE
  }
  
  if('limit' %in% names(cl)){
    cl[['all_data']] = FALSE
  }
  
  if('offset' %in% names(cl)){
    cl[['all_data']] = FALSE
  }
  ## Fixing all data line
  
  cl[['siteid']] <- siteids
  cl[['x']] <- NULL

  output <- do.call(get_sites, cl)
  
  return(output)
}
