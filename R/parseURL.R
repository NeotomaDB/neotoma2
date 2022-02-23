#' @title parseURL
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @author Simon Goring \email{ }
#' @import gtools
#' @import lubridate
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @param x The HTTP path for the particular API call.
#' @param use By default use the neotoma server (\code{neotoma}),
#' but supports either the development API server or a local server.
#' @param all_data If TRUE return all possible API calls
#' @param ... Any query parameters passed from the function calling
#' \code{parseURL}.
#' @export
parseURL <- function(x, use = "neotoma", all_data=FALSE, ...) { # nolint
  clean <- function(x) {
    ifelse(is.null(x), NA, x)
  }

  cleanNull <- function(x, fn = function(x) if (is.null(x)) NA else x) { # nolint
    if (is.list(x)) {
      lapply(x, cleanNull, fn)
    } else {
      fn(x)
    }
  }

  baseurl <- switch(use,
                    "dev" = "https://api-dev.neotomadb.org/v2.0/",
                    "neotoma" = "https://api.neotomadb.org/v2.0/",
                    "local" = "http://localhost:3005/v2.0/",
                    use)

  query <- list(...)
  response <- httr::GET(paste0(baseurl, x),
                        add_headers("User-Agent" = "neotoma2 R package"),
                        query = query)

  response_url <- response$url


  # When need to check API endpoint, uncomment below

  cl <- as.list(match.call())

  stop_for_status(response,
                  task = "Could not connect to the Neotoma API.
                  Check that the path is valid, and check the current
                   status of the Neotoma API services at
                    http://data.neotomadb.org")
  if (response$status_code == 200) {
    result <- jsonlite::fromJSON(httr::content(response, as = "text"),
                                 flatten = FALSE,
                                 simplifyVector = FALSE)
  }

  if (all_data == TRUE) {

    responses <- c()
    result <- cleanNull(result)

    result1 <- result$data

    responses <- c(responses, result1)

    if ("offset" %in% names(query)) {
      param_offset <- query$offset
      param_offset_old <- param_offset - 1
      query$offset <- NULL
    }else{
      param_offset_old <- 0
      param_offset <- length(result$data)
    }

    if ("limit" %in% names(query)) {
      stop("You cannot use the limit parameter when all_data=TRUE")
    }

    while ((length(result1) > 0) & param_offset_old != param_offset) {
      if (grepl("\\?", response_url)) {
        query$offset <- NULL
        query <- c(query, offset = param_offset)
        response <- httr::GET(paste0(response_url, "&limit=500"),
                              add_headers("User-Agent" = "neotoma2 R package"),
                              query = query)

      }else{
        query$offset <- NULL
        query <- c(query, offset = param_offset)
        response <- httr::GET(paste0(response_url, "?limit=500"),
                              add_headers("User-Agent" = "neotoma2 R package"),
                              query = query)

      }

      if (response$status_code == 200) {
        result2 <- jsonlite::fromJSON(httr::content(response, as = "text"),
                                      flatten = FALSE,
                                      simplifyVector = FALSE)
      }

      result2 <- cleanNull(result2)
      param_offset_old <- param_offset
      param_offset <- param_offset + length(result2$data)

      responses <- c(responses, result2$data)
    }

    result$data <- responses

  }else{
    result <- cleanNull(result)
  }

  message(paste0("Your search returned ", length(result$data), " objects."))
  
  return(result)
}