#' @title neotoma_baseurl
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @description An internal helper that resolves the base URL for the Neotoma
#' API. The `APIPOINT` environment variable (set by `set_server()`) takes
#' precedence over the `use` argument.
#' @param use One of "neotoma", "dev" or "local". Any other value is used as-is.
#' @returns `character` base URL for the API.
#' @noRd
neotoma_baseurl <- function(use = "neotoma") {
  apipoint <- Sys.getenv("APIPOINT")
  if (!apipoint == "") {
    use <- apipoint
  }
  switch(use,
         "dev" = "http://api-dev.neotomadb.org/v2.0/",
         "neotoma" = "https://api.neotomadb.org/v2.0/",
         "local" = "http://localhost:3001/v2.0/",
         use)
}

#' @title neotoma_headers
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr add_headers
#' @description An internal helper that returns the standard request headers.
#' @param json If TRUE, add the JSON `Content-Type` header used for POST bodies.
#' @returns `request` headers object.
#' @noRd
neotoma_headers <- function(json = FALSE) {
  if (json) {
    add_headers("User-Agent" = "neotoma2 R package",
                "Content-Type" = "application/json")
  } else {
    add_headers("User-Agent" = "neotoma2 R package")
  }
}

#' @title neotoma_body
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom jsonlite toJSON
#' @description An internal helper that builds the JSON body for a POST request.
#' If a `loc` (spatial) parameter is present it is first converted to GeoJSON
#' with `parseLocation()`.
#' @param query A named list of query parameters.
#' @returns `character` JSON body.
#' @noRd
neotoma_body <- function(query) {
  if ("loc" %in% names(query)) {
    query$loc <- parseLocation(query$loc)
  }
  toJSON(query, auto_unbox = TRUE, null = "null")
}

#' @title neotoma_id_param
#' @author Simon Goring \email{goring@wisc.edu}
#' @description An internal helper that names the identifier parameter for a
#' POST-by-id request, based on the endpoint path built by the calling
#' `get_*()` function.
#' @param x The endpoint path (e.g. "data/sites/1,2,3").
#' @returns `character` name of the id parameter ("siteid", "datasetid", ...).
#' @noRd
neotoma_id_param <- function(x) {
  parts <- strsplit(x, "/")[[1]]
  base <- sub("\\?.*$", "", paste(parts[seq_len(min(2, length(parts)))],
                                  collapse = "/"))
  if (grepl("site", base)) {
    "siteid"
  } else if (grepl("dataset|download", base)) {
    "datasetid"
  } else {
    parts[length(parts) - 1]
  }
}

#' @title neotoma_request
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal helper that runs a single API request and maps
#' low-level errors (e.g. SSL failures) to informative messages.
#' @param expr A function that performs the request and returns the response.
#' @returns The `httr` response, or stops with an informative error.
#' @noRd
neotoma_request <- function(expr) {
  tryCatch(
    expr(),
    error = function(e) {
      if (grepl("SSL certificate", conditionMessage(e), ignore.case = TRUE)) {
        stop("SSL certificate error: ", conditionMessage(e),
             "\nPlease contact the Neotoma team.")
      } else {
        stop("API request failed: ", conditionMessage(e),
             "\nCheck that the API path is valid or visit:
               http://data.neotomadb.org")
      }
    }
  )
}

#' @title neotoma_status_check
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr http_error stop_for_status
#' @description An internal helper that validates an API response, raising a
#' clear error for a missing response, a rate limit (429), or any other HTTP
#' failure.
#' @param response The `httr` response to check.
#' @returns The `response` invisibly if it is valid.
#' @noRd
neotoma_status_check <- function(response) {
  if (is.null(response)) {
    stop("Error: Check your R Code.")
  }
  if (inherits(response, "response") && http_error(response)) {
    if (response$status_code == 429) {
      stop("Error 429: Too many requests. Please wait and retry.")
    } else {
      stop_for_status(response,
                      task = "Could not connect to the Neotoma API.
                              Check that the path is valid, and
                              check the current status of the
                              Neotoma API services at
                              http://data.neotomadb.org")
    }
  }
  invisible(response)
}

#' @title neotoma_content
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @description An internal helper that parses the JSON payload of a response
#' into an R list.
#' @param response The `httr` response.
#' @returns `list` with the parsed API response.
#' @noRd
neotoma_content <- function(response) {
  fromJSON(content(response, as = "text"),
           flatten = FALSE,
           simplifyVector = FALSE)
}

#' @title neotoma_post_ids
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr POST http_error
#' @importFrom stringr str_extract_all
#' @description An internal helper that POSTs a query whose identifier list is
#' too long for a `GET`. When no other query parameters are supplied the ids are
#' chunked (50 per request) and all pages are returned; otherwise a single POST
#' is issued.
#' @param baseurl The API base URL.
#' @param x The endpoint path (e.g. "data/sites/1,2,3").
#' @param query A named list of query parameters.
#' @param chunk Number of ids to request per POST when chunking.
#' @returns `list` with cleaned and parsed data from the HTTP request.
#' @noRd
neotoma_post_ids <- function(baseurl, x, query, chunk = 50) {
  parts <- strsplit(x, "/")[[1]]
  base <- sub("\\?.*$", "", paste(parts[seq_len(min(2, length(parts)))],
                                  collapse = "/"))
  url <- paste0(baseurl, base)
  resource <- neotoma_id_param(x)
  value <- parts[length(parts)]

  if (length(query) == 0) {
    query$limit <- chunk
    ids <- as.numeric(str_extract_all(value, "[0-9.]+")[[1]])
    batches <- split(ids, ceiling(seq_along(ids) / chunk))
    results <- list()
    for (batch in batches) {
      query[[resource]] <- batch
      body <- neotoma_body(query)
      response <- neotoma_request(function() {
        POST(url, body = body, encode = "raw", neotoma_headers(json = TRUE))
      })
      if (http_error(response)) {
        warning("Skipping failed request with status ", response$status_code)
        next
      }
      r <- neotoma_content(response)
      if (!is.null(r$data)) {
        results <- c(results, r$data)
      }
    }
    return(list(status = 200, data = results, message = "Success"))
  }

  if (!resource %in% names(query)) {
    query[[resource]] <- value
  }
  body <- neotoma_body(query)
  response <- neotoma_request(function() {
    POST(url, body = body, encode = "raw", neotoma_headers(json = TRUE))
  })
  neotoma_status_check(response)
  neotoma_content(response)
}

#' @title neotoma_fetch
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr GET POST modify_url
#' @description An internal helper that performs a single (non-paginated) API
#' request. A `GET` is used for short URLs; a `POST` is used for spatial (`loc`)
#' queries and for URLs long enough to be rejected by the server.
#' @param baseurl The API base URL.
#' @param x The endpoint path.
#' @param query A named list of query parameters.
#' @returns `list` with cleaned and parsed data from the HTTP request.
#' @noRd
neotoma_fetch <- function(baseurl, x, query) {
  full_url <- modify_url(baseurl,
                         path = file.path("v2.0", x),
                         query = query[setdiff(names(query), "loc")])
  use_loc <- "loc" %in% names(query)
  too_long <- nchar(full_url) >= 2000

  if (!use_loc && !too_long) {
    response <- neotoma_request(function() {
      GET(paste0(baseurl, x), neotoma_headers(), query = query)
    })
    neotoma_status_check(response)
    return(neotoma_content(response))
  }

  if (use_loc) {
    body <- neotoma_body(query)
    parts <- strsplit(x, "/")[[1]]
    url <- paste0(baseurl, paste(parts[seq_len(min(2, length(parts)))],
                                 collapse = "/"))
    response <- neotoma_request(function() {
      POST(url, body = body, encode = "raw", neotoma_headers(json = TRUE))
    })
    neotoma_status_check(response)
    return(neotoma_content(response))
  }

  # URL too long for a GET: POST the identifier list.
  neotoma_post_ids(baseurl, x, query)
}

#' @title neotoma_paginate
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal helper that walks every page of a query, requesting
#' `page` records at a time until the API returns no more data.
#' @param baseurl The API base URL.
#' @param x The endpoint path.
#' @param query A named list of query parameters.
#' @param page Number of records to request per page.
#' @returns `list` with all pages of data combined.
#' @noRd
neotoma_paginate <- function(baseurl, x, query, page = 2000) {
  query$offset <- 0
  query$limit <- page
  responses <- list()
  repeat {
    r <- neotoma_fetch(baseurl, x, query)
    r <- cleanNULL(r)
    if (is.null(r$data) || length(r$data) == 0) {
      break
    }
    responses <- c(responses, r$data)
    query$offset <- query$offset + query$limit
  }
  list(status = 200, data = responses, message = "Success")
}

#' @title parseURL
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response. The
#' endpoint path is built by the calling `get_*()` function; `parseURL()` is the
#' transport layer that chooses `GET` vs `POST`, handles pagination, and returns
#' a parsed response.
#' @param x The HTTP/S path for the particular API call.
#' @param use Uses the Neotoma server by default ("neotoma"),
#' but supports either the development API server ("dev"),
#' or a local server ("local").
#' @param all_data If TRUE return all pages of the query.
#' @param ... Any query parameters passed from the calling function.
#' @returns `list` with cleaned and parsed data from HTTP request. The result
#' is always a list whose named elements are `status`, `data` and `message`.
#' @noRd
parseURL <- function(x, use = "neotoma", all_data = FALSE, ...) {
  baseurl <- neotoma_baseurl(use)
  query <- list(...)
  if (all_data) {
    return(neotoma_paginate(baseurl, x, query))
  }
  neotoma_fetch(baseurl, x, query)
}
