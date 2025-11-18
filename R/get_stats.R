#' @title get_stats
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom purrr map map_df
#' @description Returns a count of sites, datasets, publications and other
#' objects added to Neotoma during the requested time period.
#' @details This function returns summaries about the data holdings
#' within Neotoma using the existing Neotoma API's `summary` endpoint. This can
#' provide information about recent uploads (the number of new sites
#' uploaded within the last month, for example), or can be used to provide
#' information about the overall number of sites/datasets (using an arbitrarily
#' high value for `end`).
#' @param start The starting month (from present == 0) for
#' which to generate the summary. Default is 0 (the current month).
#' @param end The ending month (from present == 0) for which
#' to generate the summary. Default is 1 (one month ago).
#' @param type A character string indicating the type of summary
#' to return. Options are \code{dsdbmonth} (the number of datasets
#' in the Neotoma Database added per month), \code{rawbymonth} (the
#' number of datasets, sites, publications, authors, countries and observations
#' added per month), and \code{dstypemonth} (the number of datasets added
#' per dataset type per month). Default is \code{dsdbmonth}.
#' @returns `data.frame` with summary statistics
#' @examples \donttest{
#' tryCatch({
#' last_month <- get_stats(start = 0, end = 1, type = "dsdbmonth")
#' }, error = function(e) {
#'  message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @md
#' @export
get_stats <- function(start = 0, end = 1, type = "dsdbmonth") {
  if (!(type %in% c("dsdbmonth", "rawbymonth", "dstypemonth"))) {
    stop("type must be either 'dsdbmonth', 'dstypemonth' or 'rawbymonth'.")
  }
  if (type == "dsdbmonth") {
    baseURL <- paste0("data/summary/dsdbmonth?start=", start, "&end=", end)
  } else if (type == "rawbymonth") {
    baseURL <- paste0("data/summary/rawbymonth?start=", start, "&end=", end)
  } else if (type == "dstypemonth") {
    baseURL <- paste0("data/summary/dstypemonth?start=", start, "&end=", end)
  }
  result <- tryCatch(
    parseURL(baseURL),
    error = function(e) {
      message("API call failed: ", e$message)
      NULL
    }
  )
  if (is.null(result) || is.null(result$data)) {
    message("No results returned from Neotoma API.")
    return(NULL)
  }
  if (type == "dsdbmonth") {
    result <- result$data$data
    df <- result %>% map_df(~ data.frame(
      databasename = .$databasename,
      counts = as.integer(.$counts)
    ))
    return(df)
  } else if (type == "dstypemonth") {
    result <- result$data$data
    df <- result %>% map_df(~ data.frame(
      datasettype = .$datasettype,
      counts = as.integer(.$counts)
    ))
    return(df)
  } else if (type == "rawbymonth") {
    result <- result$data$data
    df <- result %>% map_df(~ data.frame(
      datasets = .$datasets,
      sites = .$sites,
      publications = .$publications,
      authors = .$authors,
      countrygpid = .$countrygpid,
      observations = .$observations
    ))
    return(df)
  }
}