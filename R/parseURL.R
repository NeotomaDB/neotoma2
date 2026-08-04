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
#' with `parseLocation()`. A `loc` that has already been converted is passed
#' through untouched, so a paginated query converts the geometry once rather
#' than on every page.
#' @param query A named list of query parameters.
#' @returns `character` JSON body.
#' @noRd
neotoma_body <- function(query) {
  if ("loc" %in% names(query)) {
    query$loc <- neotoma_location(query$loc)
  }
  toJSON(query, auto_unbox = TRUE, null = "null")
}

#' @title neotoma_location
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal helper that converts a spatial `loc` argument to the
#' GeoJSON body the API expects, and recognises a value that has already been
#' converted. `parseLocation()` round-trips the geometry through `sf`
#' (`geojson_sf()` -> `sf_geojson()` -> `fromJSON()` -> `toJSON()`), which is
#' cheap for a handful of vertices and expensive for a boundary read from a
#' shapefile. Converting once per query rather than once per page keeps that
#' cost off the pagination loop.
#' @param loc A `loc` argument, either raw (`sf`, WKT, GeoJSON, bbox) or the
#' result of an earlier `parseLocation()` call.
#' @returns `list` of length one holding the GeoJSON body.
#' @noRd
neotoma_location <- function(loc) {
  if (is_parsed_location(loc)) {
    return(loc)
  }
  parseLocation(loc)
}

#' @title is_parsed_location
#' @description An internal helper that tests whether a `loc` value is already
#' the output of `parseLocation()` -- a length-one list holding a JSON string.
#' @param loc The value to test.
#' @returns `logical` TRUE when `loc` has already been converted.
#' @noRd
is_parsed_location <- function(loc) {
  is.list(loc) && length(loc) == 1 && inherits(loc[[1]], "json")
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

#' @title neotoma_env_num
#' @description An internal helper that reads a positive number from an
#' environment variable, falling back to a default when the variable is unset
#' or cannot be read as a positive number.
#' @param name The environment variable to read.
#' @param default The value to use when `name` is unset or unusable.
#' @returns A single positive numeric value.
#' @noRd
neotoma_env_num <- function(name, default) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, "")))
  if (length(value) != 1 || is.na(value) || value <= 0) default else value
}

#' @title neotoma_terminal_status
#' @description An internal helper listing the HTTP statuses that cannot succeed
#' on a retry. A malformed or not-found request will fail the same way every
#' time, so it is raised immediately.
#' @returns `numeric` vector of terminal status codes.
#' @noRd
neotoma_terminal_status <- function() {
  c(400, 401, 403, 404, 405, 410, 422)
}

#' @title neotoma_is_timeout
#' @description An internal helper that recognises a request aborted by our own
#' client-side timeout, as distinct from a refused connection or a DNS failure.
#' @param response The `httr` response or the condition raised by the attempt.
#' @returns `logical` TRUE when the attempt timed out.
#' @noRd
neotoma_is_timeout <- function(response) {
  inherits(response, "condition") &&
    grepl("timed? ?out", conditionMessage(response), ignore.case = TRUE)
}

#' @title neotoma_should_retry
#' @importFrom httr http_error
#' @description An internal helper that decides whether an attempt is worth
#' repeating: any error, and any HTTP failure that is not terminal.
#' @param response The `httr` response or the condition raised by the attempt.
#' @returns `logical` TRUE when another attempt may succeed.
#' @noRd
neotoma_should_retry <- function(response) {
  if (inherits(response, "condition")) {
    return(TRUE)
  }
  if (!http_error(response)) {
    return(FALSE)
  }
  !(response$status_code %in% neotoma_terminal_status())
}

#' @title neotoma_retry
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr VERB timeout
#' @description An internal helper that issues a request, retrying with
#' exponential backoff when the failure is transient. Slow queries (wildcard
#' site searches, large downloads) intermittently return a gateway timeout from
#' the API, and rate limiting returns a 429; both succeed on a later attempt, so
#' they are retried rather than raised. Client errors (400-level) are returned
#' immediately -- a malformed or not-found request cannot succeed on a retry.
#'
#' Every attempt carries a timeout. Without one, a server that accepts the
#' connection and then never responds blocks forever. A timeout is retried at
#' most once, and deliberately not `times` times: the API keeps working on a
#' query we have already abandoned, so a client that retries a slow request
#' stacks concurrent work onto an endpoint that is by definition already
#' struggling. The spatial (`loc`) endpoints are slow enough that this
#' matters -- a polygon matching no sites at all still costs tens of seconds
#' server-side -- so the default timeout is set well above their observed
#' latency rather than inside it, where it would abort healthy requests
#' mid-flight.
#'
#' Both bounds can be lowered through `NEOTOMA_TIMEOUT` (seconds per attempt)
#' and `NEOTOMA_RETRIES` (attempts), which lets test runs fail fast while
#' leaving interactive use generous.
#' @param verb The HTTP verb, "GET" or "POST".
#' @param url The request URL.
#' @param ... Further arguments passed to `httr::VERB()` (headers, query,
#' body, encode).
#' @param times Maximum number of attempts, including the first.
#' @param seconds Maximum seconds to wait for any single attempt.
#' @returns The `httr` response from the last attempt.
#' @noRd
neotoma_retry <- function(verb, url, ...,
                          times = neotoma_env_num("NEOTOMA_RETRIES", 4),
                          seconds = neotoma_env_num("NEOTOMA_TIMEOUT", 180)) {
  budget <- max(1, times)
  attempt <- 1
  repeat {
    response <- tryCatch(VERB(verb, url, ..., timeout(seconds)),
                         error = function(e) e)
    if (neotoma_is_timeout(response)) {
      budget <- min(budget, 2)
    }
    if (attempt >= budget || !neotoma_should_retry(response)) {
      break
    }
    Sys.sleep(min(30, 2^attempt))
    attempt <- attempt + 1
  }
  if (neotoma_is_timeout(response)) {
    stop("The Neotoma API did not respond within ", seconds, " seconds for ",
         url, " (", attempt, " attempt(s)).\nSpatial (loc) queries are slow ",
         "server-side; try a smaller area, or raise the wait with ",
         "Sys.setenv(NEOTOMA_TIMEOUT = ", seconds * 2, ").")
  }
  if (inherits(response, "condition")) {
    stop(response)
  }
  response
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
#' @importFrom httr http_error
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
        neotoma_retry("POST", url, body = body, encode = "raw",
                      neotoma_headers(json = TRUE))
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
    neotoma_retry("POST", url, body = body, encode = "raw",
                  neotoma_headers(json = TRUE))
  })
  neotoma_status_check(response)
  neotoma_content(response)
}

#' @title neotoma_fetch
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr modify_url
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
      neotoma_retry("GET", paste0(baseurl, x), neotoma_headers(),
                    query = query)
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
      neotoma_retry("POST", url, body = body, encode = "raw",
                    neotoma_headers(json = TRUE))
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
#' `page` records at a time.
#'
#' The walk stops as soon as a page comes back shorter than the number of
#' records requested, rather than spending one further request to see an empty
#' page. On the spatial endpoints, where a single request costs tens of seconds
#' whatever it returns, that confirming request doubles the cost of any query
#' whose results fit in one page.
#'
#' `page` is deliberately large and deliberately fixed. The API's `limit` and
#' `offset` do not compose: the same query walked in smaller pages returns
#' *fewer* records overall (`sitename=Lake%` yields 1315 records at
#' `limit = 2000`, but only 605 when walked 500 at a time, because the second
#' page comes back short). Until that is fixed server-side, asking for the
#' largest page we can is what keeps results complete, so this is not a knob to
#' tune for speed.
#' @param baseurl The API base URL.
#' @param x The endpoint path.
#' @param query A named list of query parameters.
#' @param page Number of records to request per page.
#' @returns `list` with all pages of data combined.
#' @noRd
neotoma_paginate <- function(baseurl, x, query, page = 2000) {
  # Convert the geometry once, not once per page.
  if ("loc" %in% names(query)) {
    query$loc <- neotoma_location(query$loc)
  }
  query$offset <- 0
  query$limit <- page
  responses <- list()
  pages <- 0
  repeat {
    pages <- pages + 1
    if (pages > 1 && interactive()) {
      message("Fetching page ", pages, " (records ", query$offset + 1,
              "+) ...")
    }
    r <- neotoma_fetch(baseurl, x, query)
    r <- cleanNULL(r)
    if (is.null(r$data) || length(r$data) == 0) {
      break
    }
    responses <- c(responses, r$data)
    if (length(r$data) < query$limit) {
      break
    }
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
