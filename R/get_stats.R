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
#' which to generate the summary.
#' @param end The ending month (from present == 0) for which
#' to generate the summary.
#' @returns `data.frame` with summary statistics
#' @examples \donttest{
#' last_month <- get_stats(start = 0, end = 1)
#' }
#' @md
#' @export
get_stats <- function(start, end) {
  baseURL <- paste0("data/summary/rawbymonth?start=", start, "&end=", end)
  result <- tryCatch(
    parseURL(baseURL),
    error = function(e) {
      message("API call failed: ", e$message)
      NULL
    }
  )
  if (is.null(result) || is.null(result$data)) {
    return(NULL)
  } else {
    result <- result$data$data
    df <- result %>% map_df(~ data.frame(
      datasets = .$datasets,
      sites = .$sites,
      publications = .$publications,
      authors = .$authors,
      countrygpid = .$countrygpid
    ))
    return(df)
  }
}