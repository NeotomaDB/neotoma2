#' @title get_sites
#' @name get_sites
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @param x Use a single integer or vector of integers representing 
#' unique Neotoma site identifiers (siteids) to extract site information.
#' @param ... accepted arguments, see details for more information.
#' @description
#' The `get_sites` function is a wrapper for the Neotoma `sites` API
#' endpoint.
#' The function takes parameters defined by the user and returns a list
#' of site information supplied by the Neotoma Paleoecological Database.
#' The user may define all or none of the possible fields.
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
#'  * `database`  The constituent database for the record.
#' See `get_table("constituentdatabases")`
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
#' ## Find sites with a min altitude of 12m and a max altitude of 25m
#' tryCatch({
#'   sites_12to25 <- get_sites(altmin=12, altmax=25)
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#' ## Return all sites, using a minimum altitude of 2500m (returns >500 sites):
#' tryCatch({
#'   sites_2500 <- get_sites(altmin=2500, all_data = TRUE)
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#'   ## To find sites in Brazil
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' tryCatch({
#' brazil_sites <- get_sites(loc = brazil[1])
#' # Finding all sites with Liliaceae pollen in 1000 year bins:
#' lilysites <- c()
#' for (i in seq(0, 10000, by = 1000)) {
#'   lily <- get_sites(taxa=c("Liliaceae"),
#'                     ageyoung = i - 500,
#'                     ageold = i + 500,
#'                     all_data = TRUE)
#'   lilysites <- c(lilysites, length(lily))
#' }
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @md
#' @export
get_sites <- function(x = NA, ...) {
  if (missing(x)) {
    UseMethod("get_sites", "default")
  } else {
    UseMethod("get_sites", x)
  }
}

#' @rdname get_sites
#' @method get_sites numeric
#' @exportS3Method get_sites numeric
get_sites.numeric <- function(x, ...) {
  if (length(x) > 0) {
    siteids <- paste0(x, collapse = ",")
  }
  baseURL <- paste0("data/sites/", siteids)
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  if (!is.null(result) && length(result[2]$data) > 0) {
    output <- parse_site(result)
    return(output)
  } else {
    return(NULL)
  }
}

#' @rdname get_sites
#' @method get_sites default
#' @exportS3Method get_sites default
get_sites.default <- function(...) {
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  if ("siteid" %in% names(cl)) {
    # redirect to numeric method
    if ("all_data" %in% names(cl)) {
      all_data <- cl$all_data
    } else {
      all_data <- FALSE
    }
    output <- get_sites(x = cl$siteid, all_data = all_data)
    return(output)
  }
  params <- get_params("sites")
  if (!all(names(cl) %in% params)) {
    warning("Some parameters seem invalid.
             The current accepted parameters are: ",
            paste(unlist(params), collapse = ", "))
  }
  oo <- options(scipen = 9999)
  on.exit(options(oo))
  baseURL <- paste0("data/sites")
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  if (is.null(result) || is.null(result$data[1][[1]])) {
    return(NULL)
  } else {
    result <- result %>%
      cleanNULL()
    output <- parse_site(result)
    return(output)
  }
}

#' @rdname get_sites
#' @exportS3Method get_sites NULL
get_sites.NULL <- function(x, ...) {
  message("Input is NULL, returning NULL")
  return(NULL)
}