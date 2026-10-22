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
#'
#' Download responses are large, and every request costs the Neotoma API real
#' work. Pass all the dataset IDs you need to **one** call rather than calling
#' `get_downloads()` in a loop: a long vector is split into batches of 25
#' datasets and requested one after another. The answer for a set of IDs is
#' kept for the rest of the R session, so asking for the same IDs again does
#' not call the API again. Set `options(neotoma2.cache = FALSE)` to turn this
#' off.
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
#' @examples \dontrun{
#' # To find the downloads object of dataset 24:
#' tryCatch(
#'   {
#'     downloads24 <- get_downloads(24)
#'   },
#'   error = function(e) {
#'     message("Neotoma server not responding. Try again later.")
#'   }
#' )
#' # To find all downloads in Brazil
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' tryCatch(
#'   {
#'     brazil_datasets <- get_datasets(loc = brazil[1])
#'     brazil_downloads <- get_downloads(brazil_datasets)
#'   },
#'   error = function(e) {
#'     message("Neotoma server not responding. Try again later.")
#'   }
#' )
#' }
#' @md
#' @export
get_downloads <- function(x = NA, ...) {
  UseMethod("get_downloads", x)
}

# Number of datasets requested per call to the API. A download response can
# reach tens of megabytes, so a long vector is fetched in modest batches, one
# after another, rather than as one very large request.
downloads_batch_size <- 25

# Number of batch results kept in the session cache; the oldest is dropped
# first, so a long session does not hold every download in memory.
downloads_cache_max <- 20

#' @title downloads_cached
#' @description An internal helper that runs `fetch()` for one batch of dataset
#' ids and keeps the answer for the rest of the session, so asking for the same
#' ids again does not call the API again. The raw API result is cached and
#' parsed into a fresh object on every call, so callers never share an object.
#' @param key `character` identifying the batch and its query arguments.
#' @param fetch A function with no arguments that performs the request.
#' @returns The (parsed JSON) API result.
#' @noRd
downloads_cached <- function(key, fetch) {
  if (!neotoma_cache_on()) {
    return(fetch())
  }
  store <- neotoma_cache$downloads
  if (is.null(store)) {
    store <- list()
  }
  if (!is.null(store[[key]])) {
    return(store[[key]])
  }
  result <- fetch()
  store[[key]] <- result
  if (length(store) > downloads_cache_max) {
    store <- utils::tail(store, downloads_cache_max)
  }
  neotoma_cache$downloads <- store
  result
}

#' @rdname get_downloads
#' @exportS3Method get_downloads numeric
get_downloads.numeric <- function(x, ...) {
  if (length(x) == 0) {
    return(NULL)
  }
  oo <- options(scipen = 9999)
  on.exit(options(oo))
  dots <- list(...)
  ids <- unique(x)
  # A caller who sets `limit` or `offset` is slicing the result themselves, so
  # the ids are sent together and the request is left exactly as asked.
  if (any(c("limit", "offset") %in% names(dots))) {
    batches <- list(ids)
  } else {
    batches <- split(ids, ceiling(seq_along(ids) / downloads_batch_size))
  }
  records <- list()
  for (batch in batches) {
    base_url <- paste0(
      "data/downloads?datasetid=",
      paste0(batch, collapse = ",")
    )
    key <- paste(base_url, paste(names(dots), unlist(dots), collapse = "&"))
    # A failed request is raised as it is. It is not repeated with the
    # arguments removed: that is a different query, and it doubled the load on
    # a server that had just failed. Batches that succeeded stay cached, so a
    # second attempt only asks for the batches still missing.
    result <- downloads_cached(key, function() {
      tryCatch(
        do.call(parseURL, c(list(base_url), dots)),
        error = function(e) stop("API call failed: ", e$message, call. = FALSE)
      )
    })
    records <- c(records, result$data)
  }
  result <- list(status = 200, data = records, message = "Success")
  if (!is.null(result[2]$data) || (length(result[2]$data) > 0)) {
    output <- parse_site(result)
    output
  } else {
    NULL
  }
}

#' @rdname get_downloads
#' @exportS3Method get_downloads sites
get_downloads.sites <- function(x, verbose = TRUE, ...) {
  ids <- getids(x)$datasetid
  dots <- list(...)
  all_data <- if ("all_data" %in% names(dots)) dots$all_data else TRUE
  output <- get_downloads(x = ids, all_data = all_data)
  return(output)
}

## for null x
#' @rdname get_downloads
#' @exportS3Method get_downloads NULL
get_downloads.NULL <- function(x, ...) {
  NULL
}
