#' @title get_downloads
#' @name get_downloads
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom dplyr select
#' @importFrom methods new
#' @description
#' Download `sites` objects up to datum and chronology detail level.
#' @param x Use a single number to extract site information
#' @param verbose Status bar of items being downloaded
#' @param ... accepted arguments: sites, datasets
#' @details
#' The `get_downloads()` command wraps the Neotoma API
#' ([api.neotomadb.org](https://api.neotomadb.org)) call for `downloads`.
#' The call itself uses a SQL query which accepts any one of the following
#' parameters:
#'  * `datasetid` The unique dataset ID (integer) in Neotoma. Can be passed
#' as a vector of dataset IDs.
#'  * `all_data` The API only downloads the first 25 records of the query. 
#'  For the complete records, use `all_data=TRUE`
#' @returns The function returns either a single item of class
#' \code{"try-error"} describing the reason for failure
#' (either misdefined parameters or an error from the Neotoma API),
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
#' @examples \donttest{
#' # To find the downloads object of dataset 24:
#' tryCatch({
#'   downloads24 <- get_downloads(24)
#' }, error = function(e) {
#'  message("Neotoma server not responding. Try again later.")
#' })
#' # To find all downloads in Brazil
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' tryCatch({
#'   brazil_datasets <- get_datasets(loc = brazil[1])
#'   brazil_downloads <- get_downloads(brazil_datasets)
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @md
#' @export
get_downloads <- function(x, verbose = TRUE, ...) {
  UseMethod("get_downloads")
}

#' @rdname get_downloads
#' @exportS3Method get_downloads numeric
get_downloads.numeric <- function(x, ...) {
  if (length(x) > 0) {
    dataset <- paste0(x, collapse = ",")
  }
  baseURL <- paste0("data/downloads?datasetid=", dataset)
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  if (!is.null(result[2]$data) || (length(result[2]$data) > 0)) {
    output <- parse_site(result)
    return(output)
  } else {
    return(NULL)
  }
}

#' @rdname get_downloads
#' @exportS3Method get_downloads sites
get_downloads.sites <- function(x, verbose = TRUE, ...) {
  ids <- getids(x)
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  ids <- ids %>%
    select(.data$datasetid) %>%
    unique() %>%
    unlist() %>%
    as.numeric()
  if ("all_data" %in% cl) {
    all_data <- cl$all_data
  } else {
    all_data <- TRUE
  }
  output <- get_downloads(x = ids, all_data = all_data, ...)
  return(output)
}