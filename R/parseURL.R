#' @md
#' @title parseURL
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
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
parseURL <- function(x, use = "neotoma", all_data = FALSE, ...) { # nolint
  
  cleanNull <- function(x, fn = function(x) if (is.null(x)) NA else x) { # nolint
    if (is.list(x)) {
      lapply(x, cleanNull, fn)
    } else {
      fn(x)
    }
  }

  # Assign the API host location:
  if (!Sys.getenv("APIPOINT") == "") {
    use <- Sys.getenv("APIPOINT")
  }

  baseurl <- switch(use,
                    "dev" = "https://api-dev.neotomadb.org/v2.0/",
                    "neotoma" = "https://api.neotomadb.org/v2.0/",
                    "local" = "http://localhost:3005/v2.0/",
                    use)

  query <- list(...)
  if (all_data == FALSE) {
    response <- httr::GET(paste0(baseurl, x),
                          add_headers("User-Agent" = "neotoma2 R package"),
                          query = query)
    if (response$status_code == 414) {
      # The 414 error is a URL that is too long. This is a lazy way to manage
      # the choice between a POST and GET call.
      # Function with POST (Use this once server issue is resolved)
      query <- list(...)
      args <- x
      new_url <- newURL(baseurl, args, ...)
      body <- parsebody(args, ...)
      response <- httr::POST(new_url,
                             body = body,
                             add_headers("User-Agent" = "neotoma2 R package"),
                             httr::content_type("application/json"))
      warning("To get the complete data, use all_data = TRUE. 
        Returned the first 25 elements.")
    }
    
    # Break if we can't connect:
    stop_for_status(response,
                    task = "Could not connect to the Neotoma API.
                    Check that the path is valid, and check the current
                     status of the Neotoma API services at
                      http://data.neotomadb.org")

    if (response$status_code == 200) {
      result <- jsonlite::fromJSON(httr::content(response, as = "text"),
                                   flatten = FALSE,
                                   simplifyVector = FALSE)
      result <- cleanNull(result)
    }
    return(result)

  } else {
    # Here the flag all_data has been accepted, so we're going to pull
    # everything in.
    if ("limit" %in% names(query)) {
      stop("You cannot use the limit parameter when all_data is TRUE")
    }

    query$offset <- 0
    query$limit <- 100

    response <- httr::GET(paste0(baseurl, x),
                          add_headers("User-Agent" = "neotoma2 R package"),
                          query = query)
    if (response$status_code == 414) {
      # Function with Post (Use this once server issue is resolved)
      args <- x
      new_url <- newURL(baseurl, args, ...)
      body <- parsebody(args, ...)
      body <- jsonlite::fromJSON(body)


      if('siteid' %in% names(body)){
        ids_nos <- as.numeric(stringr::str_extract_all(body$siteid,
                                                       "[0-9.]+")[[1]])}
      if('datasetid' %in% names(body)){
        ids_nos <- as.numeric(stringr::str_extract_all(body$datasetid,
                                                       "[0-9.]+")[[1]])
      }
      seq_chunk <- split(ids_nos,
                         ceiling(seq_along(ids_nos) / query$limit))

      responses <- c()

      for (sequ in seq_chunk) {
        body2 <- list()
        body2 <- body
        names(body2) <- names(body)

        if('siteid' %in% names(body)){
          body2$siteid <- paste0(sequ, collapse = ",")
        }
        if('datasetid' %in% names(body)){
          body2$datasetid <- paste0(sequ, collapse = ",")
        }
        body2$limit <- 50
        body2 <- jsonlite::toJSON(body2, auto_unbox = TRUE)
        response <- httr::POST(new_url,
                               body = body2,
                               add_headers("User-Agent" = "neotoma2 R package"),
                               httr::content_type("application/json"))
        stop_for_status(response,
                        task = "Could not connect to the Neotoma API.
                    Check that the path is valid, and check the current
                     status of the Neotoma API services at
                      http://data.neotomadb.org")
        
        result <- jsonlite::fromJSON(httr::content(response, as = "text"),
                                     flatten = FALSE,
                                     simplifyVector = FALSE)
        
        responses <- c(responses, cleanNull(result)$data)
      }
      result$data <- responses
      return(result)
    } else {
      responses <- c()
      while (TRUE) {
        response <- httr::GET(paste0(baseurl, x),
                              add_headers("User-Agent" = "neotoma2 R package"),
                              query = query)
        stop_for_status(response,
                        task = "Could not connect to the Neotoma API.
                    Check that the path is valid, and check the current
                     status of the Neotoma API services at
                      http://data.neotomadb.org")
        
        result <- jsonlite::fromJSON(httr::content(response, as = "text"),
                                     flatten = FALSE,
                                     simplifyVector = FALSE)
        
        if (length(cleanNull(result)$data) == 0) {
          break
        }
        responses <- c(responses, cleanNull(result)$data)
        query$offset <- query$offset + query$limit
      }
      result$data <- responses
      return(result)
    }
  }
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
