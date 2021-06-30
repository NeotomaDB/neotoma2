#' @title Get table record from Neotoma
#' @description Call Neotoma and return a table (with limits & offsets for large tables)
#' @param x Table name (consult \url{https://open.neotomadb/dbschema} for a complete list of table names.
#' @param limit Default 25 records
#' @param offset Default 0.
#' @examples
#' # Returns only the first 25 specimen records.
#' someSpec <- get_table('specimens')
#' dim(someTaxa) 
#' # Loop with the offset to get all specimens:
#' okay <- TRUE
#' counter <- 1
#' specimens <- list()
#' while(okay) {
#'   specimens[[counter]] <- get_table('specimens', offset = (counter - 1) * 25)
#'   if(nrow(specimens[[counter]]) < 25) { 
#'     okay <- FALSE 
#'   } else {
#'     counter <- counter + 1
#'   }
#' }
#' specimens <- specimens %>% dplyr::bind_rows()
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @export
get_table <- function(x, limit =25, offset=0) {
  result <- parseURL(paste0('dbtables/table?table=', x, '&limit=', limit, '&offset=', offset))
  output <- result$data$data %>% 
    purrr::map(data.frame) %>% dplyr::bind_rows()
  return(output)
}