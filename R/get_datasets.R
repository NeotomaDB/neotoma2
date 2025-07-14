#' @title get_datasets
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @import gtools
#' @import lubridate
#' @import geojsonsf
#' @importFrom methods new
#' @description
#' The `get_datasets()` function is a wrapper for the Neotoma `datasets` API
#' endpoint.
#' The function takes parameters defined by the user and returns dataset
#' information supplied by the Neotoma Paleoecological Database.
#' The user may define all or none of the possible fields.
#' @param x A single datasetid, or a vector of unique dataset ids.
#' @param ... accepted arguments, see details for more information.
#' @details
#' A `dataset` is an element nested within `neotoma2` site objects. The
#' `get_datasets()` call returns a list of individual `site` objects with
#' `collunits` (collection units) that contain valid, matching `dataset`
#' elements.
#' So, `get_sites()` returns only site metadata. `get_datasets()` returns
#' site metadata, plus metadata about the individual datasets present at that
#' site.
#' The `get_datasets()` function searches for each site within Neotoma
#' that matches the query parameters, and returns them as a `sites` object,
#' a list of `site` objects, plus returns all the additional metadata for
#' the datasets at that site.
#' The `get_datasets()` command wraps the Neotoma API
#' ([api.neotomadb.org](https://api.neotomadb.org)) call for `datasets`.
#' The call itself uses a SQL query which accepts any one of the following
#' parameters:
#'  * `siteid`  The unique site ID (integer) in Neotoma. Can be passed as a
#' vector of site IDs.
#'  * `sitename`  The site name, or approximate match using the % wildcard.
#'  * `database`  The constituent database for the record. See 
#' `get_table("constituentdatabases")`
#'  * `datasettype` Neotoma contains data for a number of dataset types. 
#'  This returns a subset of data types. For a complete list of available 
#'  dataset types, run `neotoma2::get_table('datasettypes')`
#'  * `altmin`  The minimum altitude range for site elevation (in meters).
#'  *  `altmax`  The maximum altitude range for site elevation (in meters).
#'  *  `datasetid`  The unique dataset ID (integer) in Neotoma. Can be passed
#' as a vector of dataset IDs.
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
#'  * `all_data` The API only downloads the first 25 records of the query. 
#'  For the complete records, use `all_data=TRUE`
#' @returns The function returns either a single item of class `"try-error"`
#' describing the reason for failure (either misdefined parameters or an error
#' from the Neotoma API), or list of site objects, each containing one or more
#' `collunit` objects, with fully populated `datasets` elements.
#' @examples \donttest{
#' # To find all datasets with a min altitude of 12 and a max altitude of 25:
#' sites_12to25 <- get_datasets(altmin=12, altmax=25)
#' # To find all datasets in Brazil
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' brazil_datasets <- get_datasets(loc = brazil[1], limit=2)
#' # To obtain the dataset metadata:
#' datasets(brazil_datasets)
#' # There is insufficient metadata at this point to obtain information
#' # about taxa present at the site. We must use get_downloads() to
#' # obtain the full set of sample information:
#' # This fails: taxa(brazil_datasets)
#' }
#' @export
get_datasets <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_datasets", x)
  } else {
    UseMethod("get_datasets", NA)
  }
}

#' @title Get Dataset Numeric
#' @param x Use a single number to extract site information
#' @param ... Additional parameters to get_datasets
#' @returns `sites` object with full metadata up to the `dataset` level
#' @examples \donttest{
#' allds <- get_datasets(1:3)
#' }
#' @export
get_datasets.numeric <- function(x, ...) {
  if (length(x) > 0) {
    dataset <- paste0(x, collapse = ",")
  }
  base_url <- paste0("data/datasets/", dataset)
  result <- parseURL(base_url, ...)
  if (length(result[2]$data) > 0) {
    output <- parse_site(result)
    return(output)
  } else {
    return(NULL)
  }
}

#' @title Get Dataset Default
#' @param x Use a single number to extract dataset information
#' @param ... accepted arguments, see details for more information.
#' @importFrom utils URLencode
#' @returns `sites` object with full metadata up to the `dataset` level
#' @export
get_datasets.default <- function(x, ...) {
  params <- get_params("datasets")
  oo <- options(scipen = 9999999)
  on.exit(options(oo))
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  if (!all(names(cl) %in% params)) {
    warning("Some parameters seem invalid. The current accepted parameters are: ",
            paste(unlist(params), collapse = ", "))
  }
  base_url <- "data/datasets/"
  result <- parseURL(base_url, ...)
  if (is.null(result$data[1][[1]]) || is.null(result[1][[1]])) {
    return(NULL)
  } else {
    output <- parse_site(result)
    return(output)
  }
}

# for sites objects
#' @title Get Dataset from a \code{sites} object.
#' @param x An object of class \code{sites}.
#' @param ... additional arguments accepted by \code{get_datasets()}
#' @returns `sites` object with full metadata up to the `dataset` level
#' @examples \donttest{
#' random_sites <- get_sites(1)
#' allds <- get_datasets(random_sites, limit=3)
#' }
#' @export
get_datasets.sites <- function(x, ...) {
  ids <- getids(x)
  
  ids <- ids %>%
    dplyr::select(datasetid) %>%
    unique() %>%
    unlist() %>%
    as.numeric()

  output <- get_datasets(x = ids, all_data=TRUE, ...)
  
  return(output)
}

#' @title Get Dataset from a \code{site} object.
#' @param x An object of class \code{site}.
#' @param ... additional arguments accepted by \code{get_datasets()}
#' @returns `sites` object with full metadata up to the `dataset` level
#' @examples \donttest{
#' random_sites <- get_sites(1)
#' allds <- get_datasets(random_sites, limit=3)
#' }
#' @export
get_datasets.site <- function(x, ...) {
  # List of datasets ids
  ids1 <- getids(x)
  ids <- ids1 %>% dplyr::filter(!is.na(suppressWarnings(as.numeric(siteid))),
                                !is.na(suppressWarnings(as.numeric(datasetid))))
  
  ids2 <- getids(x) %>% dplyr::filter(is.na(suppressWarnings(as.numeric(siteid))) |
                                        is.na(suppressWarnings(as.numeric(datasetid))))
  
  if(nrow(ids2)!=0){
    warnsite <- sprintf("SiteID %s or DatasetID %s does not exist in the Neotoma DB yet or it has been removed. 
                        It will be removed from your search.",  paste0(ids2$siteid,collapse = ", "), paste0(ids2$datasetid,collapse = ", "))
    warning(warnsite)
  }
  
  dataset_list <- ids$datasetid
  dataset_list <- as.numeric(unlist(dataset_list))
  
  output <- get_datasets(dataset_list, all_data=TRUE)
  
  return(output)
}