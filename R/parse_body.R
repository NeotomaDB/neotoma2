utils::globalVariables(c("col1", "df_ready2"))
#' @title parse_body
#' @author Socorro Dominguez
#' @import gtools
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @importFrom jsonlite toJSON
#' @description An internal helper function to parse the body of POST API requests
#' @param x The HTTP path for the particular API call.
#' @param all_data recovers all_data parameter to decide how to handle downloads 
#' lists that would result in a 414 error.
#' @param ... Any query parameters passed from the function calling
#' @returns `JSON` object to parse as a body in a HTTP request
#' @keywords internal
parsebody <- function(x, all_data, ...) {
  #query <- list(...)
  # Retrieve complete call to create json body
  # There are 3 cases
  # I. Long list of IDs (most common)
  if (grepl("datasets", x)) {
    params <- stringr::str_remove_all(x, "data/datasets")
    if(substr(params, 1, 1) == "/") {
      numbers <- stringr::str_remove_all(params, "/")
      body <- jsonlite::toJSON(list(datasetid = numbers, ...))
    }
  } else if(grepl("sites", x)){
    params <- stringr::str_remove_all(x, "data/sites")
    if(substr(params, 1, 1) == "/") {
      numbers <- stringr::str_remove_all(params, "/")
      body <- jsonlite::toJSON(list(siteid = numbers, ...))
    }
  } else if(grepl("downloads", x)){
    params <- stringr::str_remove_all(x, "data/downloads\\?datasetid=")
    if(all_data==FALSE){
      elements <- strsplit(params, ",")[[1]]
      params <- paste(elements[1:25], collapse = ",")
    } 
    body <- jsonlite::toJSON(list(datasetid = params, ...))
  }
  # II. Other simple queries - Unlikely unless it comes with a complex location
  if (params == "") {
    body <- jsonlite::toJSON(list(...), flatten = TRUE)
  }
  # III. When location is present and the base_URL has too much info
  if(substr(params, 1, 1) == "?") {
    params <- stringr::str_remove_all(params, "\\?")
    params <- stringr::str_replace_all(params, "=", ":")
    params <- strsplit(params, "&")
    df <- data.frame(params)
    df <- dplyr::as_tibble(df)
    df <- df %>% dplyr::rename(col1 = colnames(df[1]))
    df2 <- df %>%
      tidyr::separate(col1, c("name", "value"), sep = ":",
                      Sremove = FALSE, extra = "merge")
    df2 <- df2 %>% select("name", "value")
    df2 <- tidyr::pivot_wider(df2)
    body <- jsonlite::toJSON(df2, auto_unbox = TRUE)
  }
  return(body)
}
