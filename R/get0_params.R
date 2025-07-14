#' @title get_datasets
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @import httr
#' @import jsonlite
#' @import stringr
#' @keywords internal
#' @noRd
get_params <- function(obj) {
  url <- "https://api.neotomadb.org/api-docs/swagger-ui-init.js"
  r <- tryCatch({
    resp <- httr::GET(url)
    result <- httr::content(resp, "text", encoding = "UTF-8")
    sw <- stringr::str_match(result,
                    paste0("v2.0/data/", obj,
                           "\"\\s*:\\s*(\\{[\\s\\S]+?\\})\\s*\\n"))[, 2]
    matches <- stringr::str_match_all(
      sw, "#/components/parameters/([a-zA-Z0-9_]+)Query\\\""
    )[[1]][, 2]
    matches <- unique(matches)
    matches
  }, error = function(e) {
    warning("Swagger with available parameters not available in API: ",
            conditionMessage(e))
    NULL
  })
  r <- append(r, list("all_data", "limit", "offset"))
  r <- lapply(r, tolower)
  return(r)
}