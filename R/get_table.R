#' @title Get table record from Neotoma
#' @description Call Neotoma and return a table
#'  (with limits & offsets for large tables)
#' @param x Table name (consult \url{https://open.neotomadb.org/dbschema/}
#'  for a complete list of table names.
#' @param limit Default 25 records
#' @param offset Default 0.
#' @returns selected `table` values from the Database.
#' @examples {
#' # Returns only the first 25 specimen records.
#' tryCatch({
#' someSpec <- get_table('specimens')
#' }, error = function(e) {
#'  message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @export
get_table <- function(x, limit = 25, offset = 0) {
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
    return(output)
  }
}