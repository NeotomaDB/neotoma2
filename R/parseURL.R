#' @md
#' @title parseURL
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @import stringr
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @param x The HTTP/S path for the particular API call.
#' @param use Uses the Neotoma server by default ("neotoma"),
#' but supports either the development API server ("dev"),
#' or a local server ("local").
#' @param all_data If TRUE return all possible API calls
#' @param ... Any query parameters passed from the calling function.
#' @returns `list` with cleaned and parsed data from HTTP request
#' @keywords internal
#' @noRd
parseURL <- function(x, use = "neotoma", all_data = FALSE, ...) {
  cleanNull <- function(x, fn = function(x) if (is.null(x)) NA else x) {
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
                    "dev" = "http://api-dev.neotomadb.org/v2.0/",
                    "neotoma" = "https://api.neotomadb.org/v2.0/",
                    "local" = "http://localhost:3005/v2.0/",
                    use)
  query <- list(...)

  get_response <- function(baseurl, x, query) {
    response <- tryCatch({
      full_url <- httr::modify_url(baseurl,
                                   path = file.path("v2.0", x),
                                   query = query[setdiff(names(query), "loc")])
      if (nchar(full_url) < 2000 && !('loc' %in% names(query))) {
        httr::GET(paste0(baseurl, x),
                  httr::add_headers("User-Agent" = "neotoma2 R package"),
                  query = query)
      } else if ("loc" %in% names(query)) {
        query$loc <- parseLocation(query$loc)
        body <- jsonlite::toJSON(query, auto_unbox = TRUE, null = "null")
        parts <- strsplit(x, "/")[[1]]
        path_str <- paste(parts[1:2], collapse = "/")
        baseurl <- paste0(baseurl, path_str)
        response <- httr::POST(paste0(baseurl),
                               body = body,
                               encode = "raw",
                               httr::add_headers("User-Agent" =
                                                   "neotoma2 R package",
                                                 "Content-Type" =
                                                   "application/json"))
      } else {
        # Numeric Long calls
        parts <- strsplit(x, "/")[[1]]
        path_str <- paste(parts[1:2], collapse = "/")
        parts2 <- sub("\\?.*$", "", path_str)
        baseurl <- paste0(baseurl, parts2)
        if (length(parts) >= 2) {
          resource <- parts[length(parts) - 1]
          value <- parts[length(parts)] # Value limited to Site/DS IDs
          if (grepl("site", parts2)) {
            resource <- "siteid"
          } else if (any(grepl("dataset|download", parts2))) {
            resource <- "datasetid"
          }
        }
        if (length(query) == 0) {
          query$limit <- 50
          value <-  as.numeric(stringr::str_extract_all(value, "[0-9.]+")[[1]])
          seq_chunk <- split(value,
                             ceiling(seq_along(value) / query$limit))
          results <- list()
          for (sequ in seq_chunk) {
            query[[resource]] <- sequ
            body <- jsonlite::toJSON(query, auto_unbox = TRUE, null = "null")
            response <- httr::POST(paste0(baseurl),
                                   body = body,
                                   encode = "raw",
                                   httr::add_headers("User-Agent" =
                                                       "neotoma2 R package",
                                                     "Content-Type" =
                                                       "application/json"),
                                   httr::verbose())
            if (httr::http_error(response)) {
              warning("Skipping failed request with status ",
                      response$status_code)
              next
            }
            r <- jsonlite::fromJSON(httr::content(response, as = "text"),
                                    flatten = FALSE, simplifyVector = FALSE)
            
            if (!is.null(r$data)) {
              results <- c(results, r$data)
            }
          }
          response <- list(status = 200,
                           data = results,
                           message = "Success")
          return(response)
        } else {
          # Convert query to JSON
          if (!resource %in% names(query)) {
            query[[resource]] <- value
          }
          body <- jsonlite::toJSON(query, auto_unbox = TRUE, null = "null")
          httr::POST(paste0(baseurl),
                     body = body,
                     encode = "raw",
                     httr::add_headers("User-Agent" = "neotoma2 R package",
                                       "Content-Type" = "application/json"))
        }
      }
    }, error = function(e) {
      if (grepl("SSL certificate", conditionMessage(e), ignore.case = TRUE)) {
        stop("SSL certificate error: ", conditionMessage(e),
             "\nPlease contact the Neotoma team.")
      }
      stop("API request failed: ", conditionMessage(e),
           "\nCheck that the API path is valid or visit:
           http://data.neotomadb.org")
      return(NULL)
    })

    if (is.null(response) || isTRUE(inherits(response, "response") && httr::http_error(response))) {
      if (is.null(response)) {
        stop("Error: Check your R Code.")
      }
      if (response$status_code == 429) {
        stop("Error 429: Too many requests. Please wait and retry.")
      } else {
        httr::stop_for_status(response,
                              task = "Could not connect to the Neotoma API.
                                    Check that the path is valid, and 
                                    check the current status of the
                                    Neotoma API services at 
                                    http://data.neotomadb.org")
      }
    }
    return(response)
  }

  if (all_data == FALSE) {
    response <- get_response(baseurl, x, query)
    if (inherits(response, "response")) {
    result <- jsonlite::fromJSON(httr::content(response, as = "text"),
                                 flatten = FALSE,
                                 simplifyVector = FALSE)
    } else {
      result <- response
    }
  } else {
    query$offset <- 0
    query$limit <- 2000
    responses <- c()
    while (TRUE) {
      response <- get_response(baseurl, x, query)
      r <- jsonlite::fromJSON(httr::content(response, as = "text"),
                                   flatten = FALSE,
                                   simplifyVector = FALSE)
      r <- cleanNull(r)
      if (is.null(r$data) || length(r$data) == 0) {
        break
      }
      responses <- c(responses, r$data)
      query$offset <- query$offset + query$limit
    }
    result <- list(status = 200,
                     data = responses,
                     message = "Success")
  }
  return(result)
}