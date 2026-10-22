#' @title Get table record from Neotoma
#' @description Call Neotoma and return a table
#'  (with limits & offsets for large tables)
#' @param x Table name (consult \url{https://open.neotomadb.org/dbschema/}
#'  for a complete list of table names.
#' @param limit Default 25 records
#' @param offset Default 0.
#' @param refresh Default `FALSE`. Tables in the Neotoma database change rarely,
#'  so the answer to a query is kept for the rest of the R session and asking
#'  for the same `x`, `limit` and `offset` again does not call the API again.
#'  Use `refresh = TRUE` to ask the API again. The session cache can be turned
#'  off with `options(neotoma2.cache = FALSE)`.
#' @returns selected `table` values from the Database.
#' @examples \dontrun{
#' # Returns only the first 25 specimen records.
#' someSpec <- get_table('specimens')
#' }
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @export
get_table <- function(x, limit = 25, offset = 0, refresh = FALSE) {
  key <- paste("table", x, limit, offset, sep = "|")
  if (!refresh && neotoma_cache_on() && !is.null(neotoma_cache[[key]])) {
    return(neotoma_cache[[key]])
  }
  result <- tryCatch(
    parseURL("dbtables/table", table = x, limit = limit, offset = offset),
    error = function(e) {
      message("API call failed: ", e$message)
      NULL
    }
  )
  if (is.null(result$data)) {
    return(NULL)
  } else {
    result <- result$data$data
    output <- result %>%
      cleanNULL() %>%
      map(data.frame) %>%
      bind_rows()
    # Only a successful answer is kept, so a failed call is tried again.
    if (neotoma_cache_on()) {
      neotoma_cache[[key]] <- output
    }
    return(output)
  }
}
