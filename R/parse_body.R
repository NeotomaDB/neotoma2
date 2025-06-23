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
  query <- list(...)
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
    m <- list(...)
    body <- jsonlite::toJSON(m)
  }
  # III. When location is present and the base_URL has too much info
  if (startsWith(params, "?")) {
    param_list <- strsplit(substring(params, 2), "&")[[1]]
    kv_pairs <- strsplit(param_list, "=")
    # Keep "loc" as a raw string; parse others
    param_data <- list()
    for (kv in kv_pairs) {
      name <- kv[[1]]
      value <- kv[[2]]
      if (name == "loc") {
        # Preserve original percent-encoded string
        decoded_loc <- jsonlite::fromJSON(utils::URLdecode(value))
        param_data[[name]] <- decoded_loc
        #param_data[[name]] <- utils::URLdecode(value)
        #param_data[[name]] <- value
      } else {
        # Convert numeric where possible
        num_val <- suppressWarnings(as.numeric(value))
        param_data[[name]] <- if (!is.na(num_val)) num_val else value
      }
    }
    
    # Merge with ... arguments
    full_params <- utils::modifyList(param_data, query)
    body <- jsonlite::toJSON(full_params, auto_unbox = TRUE)
  }
  
  return(body)
}
