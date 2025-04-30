#' @md
#' @title parseURL
#' @author Socorro Dominguez \email{s.dominguez@ht-data.com}
#' @author Simon Goring \email{goring@wisc.edu}
#' @import gtools
#' @import lubridate
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @param x The HTTP/S path for the particular API call.
#' @param use Uses the Neotoma server by default ("neotoma"), but supports either the
#' development API server ("dev") or a local server ("local").
#' @param all_data If TRUE return all possible API calls
#' @param ... Any query parameters passed from the calling function.
#' @returns `list` with cleaned and parsed data from HTTP request
#' @export
parseURL <- function(x, use = "neotoma", all_data = FALSE, fetch_fn = NULL, ...) {
  
  # Helper to clean NULLs from JSON structures
  cleanNull <- function(x, fn = function(x) if (is.null(x)) NA else x) {
    if (is.list(x)) {
      lapply(x, cleanNull, fn)
    } else {
      fn(x)
    }
  }
  
  # Determine API base URL
  if (!Sys.getenv("APIPOINT") == "") {
    use <- Sys.getenv("APIPOINT")
  }
  
  baseurl <- switch(use,
                    "dev" = "http://api-dev.neotomadb.org/v2.0/",
                    "neotoma" = "https://api.neotomadb.org/v2.0/",
                    "local" = "http://localhost:3005/v2.0/",
                    use
  )
  
  query <- list(...)
  url <- paste0(baseurl, x)
  
  # Assign fetch function if in webR
  if (is.null(fetch_fn) && isTRUE(getOption("webr"))) {
    fetch_fn <- fetchJS
  }
  
  # If a fetch_fn is provided (likely webR environment), use it
  if (!is.null(fetch_fn)) {
    body <- if (length(query) > 0) jsonlite::toJSON(query, auto_unbox = TRUE) else NULL
    result_json <- fetch_fn(url, body)
    result <- jsonlite::fromJSON(result_json, flatten = FALSE, simplifyVector = FALSE)
    return(cleanNull(result))
  }
  
  # Otherwise, fall back to standard httr-based flow
  try(
    response <- httr::GET(url,
                          httr::add_headers("User-Agent" = "neotoma2 R package"),
                          query = query
    )
  )
  
  if (inherits(response, "try-error")) {
    error_message <- conditionMessage(response)
    if (grepl("SSL certificate", error_message, ignore.case = TRUE)) {
      stop("SSL certificate error: ", error_message)
    }
  }
  
  # Handle 414 by switching to POST
  if (response$status_code == 414) {
    new_url <- newURL(baseurl, x, ...)
    body <- parsebody(x, all_data = FALSE, ...)
    try(
      response <- httr::POST(new_url,
                             body = body,
                             httr::add_headers("User-Agent" = "neotoma2 R package"),
                             httr::content_type("application/json")
      )
    )
    stop_for_status(response, task = "Could not connect to the Neotoma API.")
  } else {
    stop_for_status(response, task = "Could not connect to the Neotoma API.")
  }
  
  result <- jsonlite::fromJSON(httr::content(response, as = "text"),
                               flatten = FALSE,
                               simplifyVector = FALSE)
  return(cleanNull(result))
}

#' @title Format API call to Neotoma from call arguments
#' @param baseurl The base URL for the Neotoma API
#' @param args The set of query arguments to be passed to the API
#' @param ... Any additional arguments to be passed to the function.
#' @description
#' Take a set of arguments from the Neotoma2 package and produce
#' the appropriate URL to the Neotoma v2.0 API.
#' This is an internal function used by `parseURL()`.
#' @returns A properly formatted URL.
newURL <- function(baseurl, args, ...) {
  query <- list(...)
  # Retrieve complete call to create json body
  # There are 3 cases
  # I. Long list of IDs (most common)
  if (grepl("datasets", args)) {
    new_url <- paste0(baseurl, "data/datasets")
    params <- stringr::str_remove_all(args, "data/datasets")
  } else if (grepl("sites", args)) {
    new_url <- paste0(baseurl, "data/sites")
    params <- stringr::str_remove_all(args, "data/sites")
  } else if (grepl("downloads", args)) {
    new_url <- paste0(baseurl, "data/downloads")
    params <- stringr::str_remove_all(args, "data/downloads")
  }
  return(new_url)
}


fetchJS <- function(url, body = NULL, method = NULL) {
  js_code <- if (!is.null(body)) {
    sprintf(
      "await fetch('%s', {
        method: '%s',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(%s)
      }).then(res => res.text())",
      url,
      ifelse(is.null(method), "POST", toupper(method)),
      body
    )
  } else {
    sprintf(
      "await fetch('%s', {
        method: '%s',
        headers: { 'Content-Type': 'application/json' }
      }).then(res => res.text())",
      url,
      ifelse(is.null(method), "GET", toupper(method))
    )
  }
  
  result <- webR::runjs(js_code)
  return(result)
}