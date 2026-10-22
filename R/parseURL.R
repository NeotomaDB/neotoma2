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
    "dev" = "https://api-dev.neotomadb.org/v2.0/",
    "neotoma" = "https://api.neotomadb.org/v2.0/",
    "local" = "http://localhost:3001/v2.0/",
    use
  )
}

#' @title neotoma_cache
#' @description An internal, session-long store for results that are expensive
#' to fetch and do not change while R is running: static database tables
#' (`get_table()`) and dataset downloads (`get_downloads()`). Keeping them here
#' means a script that asks for the same thing twice costs the API one request,
#' not two. It is emptied when R restarts, and with `neotoma_cache_clear()`.
#' Set `options(neotoma2.cache = FALSE)` to switch it off.
#' @noRd
neotoma_cache <- new.env(parent = emptyenv())

#' @title neotoma_cache_on
#' @description An internal helper: TRUE unless the user has switched the
#' session cache off with `options(neotoma2.cache = FALSE)`.
#' @returns `logical`
#' @noRd
neotoma_cache_on <- function() {
  !identical(getOption("neotoma2.cache", TRUE), FALSE)
}

#' @title neotoma_cache_clear
#' @description Empties the session cache.
#' @returns `NULL`, invisibly.
#' @noRd
neotoma_cache_clear <- function() {
  rm(list = ls(neotoma_cache, all.names = TRUE), envir = neotoma_cache)
  invisible(NULL)
}

#' @title neotoma_user_agent
#' @description An internal helper that builds the User-Agent string:
#' `neotoma2/<version> (R <x.y.z>)`, with `; contact: <address>` appended when
#' the user has set `options(neotoma2.contact = "name@example.org")` or the
#' `NEOTOMA_CONTACT` environment variable. A self-identifying client is the
#' easiest for the API team to reach if a script misbehaves. The version is
#' looked up once per session.
#' @returns `character` User-Agent value.
#' @noRd
neotoma_user_agent <- function() {
  if (is.null(neotoma_cache$user_agent)) {
    version <- tryCatch(as.character(utils::packageVersion("neotoma2")),
      error = function(e) "unknown"
    )
    neotoma_cache$user_agent <- paste0(
      "neotoma2/", version, " (R ",
      getRversion()
    )
  }
  contact <- getOption("neotoma2.contact", Sys.getenv("NEOTOMA_CONTACT", ""))
  has_contact <- is.character(contact) && length(contact) == 1 &&
    !is.na(contact) && nzchar(contact)
  if (has_contact) {
    paste0(neotoma_cache$user_agent, "; contact: ", contact, ")")
  } else {
    paste0(neotoma_cache$user_agent, ")")
  }
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
    add_headers(
      "User-Agent" = neotoma_user_agent(),
      "Content-Type" = "application/json"
    )
  } else {
    add_headers("User-Agent" = neotoma_user_agent())
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
#' time, so it is raised immediately. So does a 500: the API answers 500 for a
#' request it cannot process (for example a malformed id list), which a repeat
#' will not change, while a crash that a retry could outlive surfaces as a
#' gateway status (see `neotoma_busy_status()`).
#' @returns `numeric` vector of terminal status codes.
#' @noRd
neotoma_terminal_status <- function() {
  c(400, 401, 403, 404, 405, 410, 422, 500)
}

#' @title neotoma_busy_status
#' @description An internal helper listing the gateway statuses that mean the
#' server is overloaded or restarting (502, 503, 504). The API usually keeps
#' working on a query after the gateway gives up on it, so an immediate repeat
#' only stacks a second heavy query on top of the first. These are retried at
#' most once, after a long wait.
#' @returns `numeric` vector of status codes.
#' @noRd
neotoma_busy_status <- function() {
  c(502, 503, 504)
}

#' @title neotoma_is_busy
#' @description An internal helper that recognises a gateway "server busy"
#' response.
#' @param response The `httr` response or the condition raised by the attempt.
#' @returns `logical` TRUE for a 502, 503 or 504 response.
#' @noRd
neotoma_is_busy <- function(response) {
  inherits(response, "response") &&
    response$status_code %in% neotoma_busy_status()
}

#' @title neotoma_retry_after
#' @description An internal helper that reads a `Retry-After` header given as a
#' number of seconds. A date, a missing header and a nonsense value all return
#' `NA`. The wait is capped at two minutes so a misconfigured server cannot
#' park a script indefinitely.
#' @param response The `httr` response or the condition raised by the attempt.
#' @returns `numeric` seconds to wait, or `NA`.
#' @noRd
neotoma_retry_after <- function(response) {
  if (!inherits(response, "response")) {
    return(NA_real_)
  }
  value <- response$headers[["retry-after"]]
  if (is.null(value)) {
    return(NA_real_)
  }
  seconds <- suppressWarnings(as.numeric(value))
  if (length(seconds) != 1 || is.na(seconds) || seconds < 0) {
    return(NA_real_)
  }
  min(seconds, 120)
}

#' @title neotoma_sleep
#' @description An internal helper that pauses. It exists so that tests can
#' check how long the client would wait without actually waiting.
#' @param seconds Number of seconds to pause.
#' @noRd
neotoma_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

#' @title neotoma_wait
#' @description An internal helper that sleeps between attempts. A `Retry-After`
#' header is honoured as sent. Otherwise a busy gateway (502/503/504) gets a
#' long, jittered wait of 30 to 60 seconds, and any other transient failure an
#' exponential backoff with jitter, so that many clients that failed together
#' do not retry together. Kept as its own function so tests can replace it.
#' @param response The `httr` response or the condition raised by the attempt.
#' @param attempt The number of attempts made so far.
#' @returns The number of seconds slept, invisibly.
#' @noRd
neotoma_wait <- function(response, attempt) {
  seconds <- neotoma_retry_after(response)
  if (is.na(seconds)) {
    seconds <- if (neotoma_is_busy(response)) {
      stats::runif(1, 30, 60)
    } else {
      min(30, 2^attempt) * stats::runif(1, 0.75, 1.25)
    }
  }
  if (neotoma_is_busy(response) || seconds >= 10) {
    message(
      "The Neotoma API is busy; waiting ", round(seconds),
      " s before trying again."
    )
  }
  neotoma_sleep(seconds)
  invisible(seconds)
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
#' repeating: a connection error, and an HTTP failure that is not terminal
#' (429 rate limiting and 502/503/504 gateway failures).
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
#' a wait when the failure is transient. Rate limiting (429) succeeds on a later
#' attempt, so it is retried, waiting as long as the `Retry-After` header asks.
#' Client errors (400-level) and a 500 are returned immediately -- a malformed
#' or not-found request cannot succeed on a retry. A gateway failure
#' (502/503/504) is retried once, after a long jittered wait (see
#' `neotoma_wait()`).
#'
#' Every attempt carries a timeout. Without one, a server that accepts the
#' connection and then never responds blocks forever. A timeout, like a gateway
#' failure, is retried at most once, and deliberately not `times` times: the API
#' keeps working on a query we have already abandoned, so a client that retries
#' a slow request stacks concurrent work onto an endpoint that is by definition
#' already struggling. The spatial (`loc`) endpoints are slow enough that this
#' matters -- a polygon matching no sites at all still costs tens of seconds
#' server-side. The default timeout (125 s) sits just above the 120 s at which
#' the API gateway gives up, so the client and the gateway agree and a slow
#' query is reported once rather than abandoned and repeated.
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
                          seconds = neotoma_env_num("NEOTOMA_TIMEOUT", 125)) {
  budget <- max(1, times)
  attempt <- 1
  repeat {
    response <- tryCatch(VERB(verb, url, ..., timeout(seconds)),
      error = function(e) e
    )
    if (neotoma_is_timeout(response) || neotoma_is_busy(response)) {
      budget <- min(budget, 2)
    }
    if (attempt >= budget || !neotoma_should_retry(response)) {
      break
    }
    neotoma_wait(response, attempt)
    attempt <- attempt + 1
  }
  if (neotoma_is_timeout(response)) {
    stop(
      "The Neotoma API did not respond within ", seconds, " seconds for ",
      url, " (", attempt, " attempt(s)).\nSpatial (loc) queries are slow ",
      "server-side; try a smaller area, or raise the wait with ",
      "Sys.setenv(NEOTOMA_TIMEOUT = ", seconds * 2, ")."
    )
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
        stop(
          "SSL certificate error: ", conditionMessage(e),
          "\nPlease contact the Neotoma team."
        )
      } else {
        stop(
          "API request failed: ", conditionMessage(e),
          "\nCheck that the API path is valid or visit:
               http://data.neotomadb.org"
        )
      }
    }
  )
}

#' @title neotoma_status_check
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr http_error stop_for_status
#' @description An internal helper that validates an API response, raising a
#' clear error for a missing response, a rate limit (429), a busy server
#' (502/503/504), or any other HTTP failure.
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
    } else if (neotoma_is_busy(response)) {
      stop("Error ", response$status_code, ": the Neotoma API is busy or ",
        "restarting. Try again later, or reduce the size of the query ",
        "(fewer ids, a smaller area, a narrower taxon or name filter).",
        call. = FALSE
      )
    } else {
      detail <- neotoma_api_message(response)
      tryCatch(
        stop_for_status(response,
          task = "Could not connect to the Neotoma API.
                                Check that the path is valid, and
                                check the current status of the
                                Neotoma API services at
                                http://data.neotomadb.org"
        ),
        error = function(e) {
          stop(conditionMessage(e),
            if (!is.na(detail)) paste0("\nThe API said: ", detail),
            call. = FALSE
          )
        }
      )
    }
  }
  invisible(response)
}

#' @title neotoma_api_message
#' @description An internal helper that reads the reason the API gives for a
#' failed request. The API answers a failure with
#' `{status: "failure", data: null, message: <database error>}`. Older versions
#' put the database error in `data` and a generic sentence in `message`, so a
#' string in `data` is preferred when there is one. A body that is not JSON (an
#' HTML error page, an empty gateway reply) gives `NA`.
#' @param response The failed `httr` response.
#' @returns `character` message, or `NA_character_`.
#' @noRd
neotoma_api_message <- function(response) {
  parsed <- tryCatch(
    fromJSON(content(response, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )
  if (!is.list(parsed)) {
    return(NA_character_)
  }
  one_string <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  }
  if (one_string(parsed$data)) {
    parsed$data
  } else if (one_string(parsed$message)) {
    parsed$message
  } else {
    NA_character_
  }
}

#' @title neotoma_content
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @description An internal helper that parses the JSON payload of a response
#' into an R list. An HTML body (what the API's catch-all redirect returns) is
#' reported as such, rather than failing inside `fromJSON()`.
#' @param response The `httr` response.
#' @returns `list` with the parsed API response.
#' @noRd
neotoma_content <- function(response) {
  text <- content(response, as = "text", encoding = "UTF-8")
  if (grepl("^\\s*<", text)) {
    stop("The Neotoma API returned a web page instead of data, which usually ",
      "means the request was redirected (for example to the API ",
      "documentation). Check that the endpoint accepts this kind of ",
      "request: ", response$url,
      call. = FALSE
    )
  }
  fromJSON(text, flatten = FALSE, simplifyVector = FALSE)
}

#' @title neotoma_get
#' @importFrom httr http_error
#' @description An internal helper that issues one `GET` and returns the parsed
#' body.
#' @param baseurl The API base URL.
#' @param x The endpoint path.
#' @param query A named list of query parameters.
#' @returns `list` with the parsed API response.
#' @noRd
neotoma_get <- function(baseurl, x, query) {
  response <- neotoma_request(function() {
    neotoma_retry("GET", paste0(baseurl, x), neotoma_headers(),
      query = query
    )
  })
  neotoma_status_check(response)
  neotoma_content(response)
}

#' @title neotoma_post_route
#' @description An internal helper that tells whether the API accepts a `POST`
#' for an endpoint. Only the site, dataset and download queries do (and the
#' `spatial` ones, which this package does not call): a body lets a query carry
#' a long spatial `loc`, a long list of ids or a long list of names that would
#' not fit in a URL. Taxa, publications and contacts have no POST route, and a
#' POST to them is redirected to the API documentation.
#' @param x The endpoint path, with or without a query string.
#' @returns `logical` TRUE when the endpoint accepts a `POST`.
#' @noRd
neotoma_post_route <- function(x) {
  path <- sub("\\?.*$", "", x)
  grepl("^data/(sites|datasets|downloads)/?$", path) ||
    grepl("^data/datasets/[^/]+$", path) ||
    grepl("^data/(sites|geopoliticalunits)/[^/]+/datasets$", path) ||
    grepl("^data/(datasets|geopoliticalunits|contacts)/[^/]+/sites$", path)
}

#' @title neotoma_post
#' @importFrom httr config
#' @description An internal helper that issues one `POST`, with the query as a
#' JSON body, and returns the parsed answer. The request keeps its method and
#' body if the server redirects it (`postredir`). Without that, `libcurl`
#' follows a 301 or 302 with a `GET` and drops the body, so the API sees a query
#' with no filter at all and answers with its first page of records, which looks
#' like a valid answer. That is what a `POST` to `http://api-dev...` used to do:
#' the dev server redirects to `https`.
#' @param url The full request URL.
#' @param query A named list of parameters, sent as the JSON body.
#' @returns `list` with the parsed API response.
#' @noRd
neotoma_post <- function(url, query) {
  body <- neotoma_body(query)
  response <- neotoma_request(function() {
    neotoma_retry("POST", url,
      body = body, encode = "raw",
      neotoma_headers(json = TRUE), config(postredir = 7)
    )
  })
  neotoma_status_check(response)
  neotoma_content(response)
}

#' @title neotoma_ids_applied
#' @description An internal helper that checks the answer to a request for a
#' list of ids really came from those ids. A server that ignores the request
#' body answers with its default page of records, which looks like data. The
#' answer is rejected when it holds more records than ids were asked for, or a
#' record whose own id (`siteid`, `datasetid`) is not among them.
#' @param records The `data` element of the answer.
#' @param key The id field to compare, "siteid" or "datasetid".
#' @param ids `character` ids that were requested.
#' @returns `logical` FALSE when the answer cannot have come from the ids.
#' @noRd
neotoma_ids_applied <- function(records, key, ids) {
  if (length(records) > length(ids)) {
    return(FALSE)
  }
  found <- unlist(lapply(records, function(r) {
    if (is.list(r) && !is.null(r[[key]])) as.character(r[[key]]) else NULL
  }))
  all(found %in% ids)
}

#' @title neotoma_get_ids
#' @description An internal helper for an identifier list that is too long for
#' one `GET`. The ids are requested `chunk` at a time, one request after
#' another, and the records are combined. The list may end the path
#' (`data/taxa/1,2,3`) or follow a parameter (`data/downloads?datasetid=1,2,3`);
#' a path that ends in neither is requested as it stands.
#'
#' Where the API has a POST route (sites, datasets and downloads) each chunk is
#' sent as a `POST` whose body holds the ids and the rest of the query. Where it
#' has none (taxa, publications, contacts) each chunk is a `GET`. A chunk that
#' returns more records than the ids it asked for was not filtered by them, so
#' it is reported rather than returned.
#' @param baseurl The API base URL.
#' @param x The endpoint path (e.g. "data/taxa/1,2,3").
#' @param query A named list of query parameters.
#' @param chunk Number of ids to request at a time.
#' @returns `list` with `status`, the combined `data`, and `message`.
#' @noRd
neotoma_get_ids <- function(baseurl, x, query, chunk = 100) {
  if (!grepl("^.*[=/][0-9][0-9,.]*$", x)) {
    return(neotoma_get(baseurl, x, query))
  }
  prefix <- sub("[0-9][0-9,.]*$", "", x)
  endpoint <- sub("(/|\\?[A-Za-z]+=)$", "", prefix)
  ids <- strsplit(sub("^.*[=/]", "", x), ",")[[1]]
  batches <- split(ids, ceiling(seq_along(ids) / chunk))
  use_post <- neotoma_post_route(endpoint)
  id_key <- if (grepl("^data/sites", endpoint)) "siteid" else "datasetid"
  results <- list()
  for (batch in batches) {
    if (use_post) {
      query[[id_key]] <- paste(batch, collapse = ",")
      r <- neotoma_post(paste0(baseurl, endpoint), query)
      if (!neotoma_ids_applied(r$data, id_key, batch)) {
        stop("The Neotoma API returned records that do not match the ",
          length(batch), " ids requested from ", endpoint, ". The ids ",
          "were not applied to the query.",
          call. = FALSE
        )
      }
    } else {
      r <- neotoma_get(
        baseurl, paste0(prefix, paste(batch, collapse = ",")),
        query
      )
    }
    if (!is.null(r$data)) {
      results <- c(results, r$data)
    }
  }
  list(status = 200, data = results, message = "Success")
}

#' @title neotoma_fetch
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr modify_url
#' @description An internal helper that performs a single (non-paginated) API
#' request. A `GET` is used unless the query has a spatial `loc`, which is sent
#' as a `POST` body. A URL too long to send is either a long list of ids,
#' which is split into several requests (see `neotoma_get_ids()`), or a long
#' list of filter values. The latter is sent as a `POST` body where the
#' endpoint has a POST route, and otherwise as the `GET` it is.
#' @param baseurl The API base URL.
#' @param x The endpoint path.
#' @param query A named list of query parameters.
#' @returns `list` with cleaned and parsed data from the HTTP request.
#' @noRd
neotoma_fetch <- function(baseurl, x, query) {
  full_url <- modify_url(baseurl,
    path = file.path("v2.0", x),
    query = query[setdiff(names(query), "loc")]
  )

  if ("loc" %in% names(query)) {
    parts <- strsplit(x, "/")[[1]]
    url <- paste0(baseurl, paste(parts[seq_len(min(2, length(parts)))],
      collapse = "/"
    ))
    return(neotoma_post(url, query))
  }

  if (nchar(full_url) >= 2000) {
    if (grepl("^.*[=/][0-9][0-9,.]*$", x)) {
      return(neotoma_get_ids(baseurl, x, query))
    }
    if (neotoma_post_route(x)) {
      return(neotoma_post(paste0(baseurl, sub("\\?.*$", "", x)), query))
    }
  }
  neotoma_get(baseurl, x, query)
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
      message(
        "Fetching page ", pages, " (records ", query$offset + 1,
        "+) ..."
      )
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

#' @title neotoma_flatten_query
#' @description An internal helper that joins every multi-valued query
#' parameter into one comma-separated value (`datasettype = c("pollen",
#' "macrofossil")` becomes `datasettype=pollen,macrofossil`). A URL query holds
#' one value per parameter, and the API reads such a list with a comma
#' separator. Without this, building the URL stopped with "values must be
#' length 1" for any vector argument, and the `get_*()` function reported it as
#' a failed API call. `loc` is left alone: it is converted to a GeoJSON body
#' by `neotoma_body()`.
#' @param query A named list of query parameters.
#' @returns The `query`, with atomic vectors of length two or more collapsed.
#' @noRd
neotoma_flatten_query <- function(query) {
  for (name in setdiff(names(query), "loc")) {
    value <- query[[name]]
    if (is.atomic(value) && length(value) > 1) {
      query[[name]] <- paste(value, collapse = ",")
    }
  }
  query
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
  query <- neotoma_flatten_query(list(...))
  if (all_data) {
    return(neotoma_paginate(baseurl, x, query))
  }
  neotoma_fetch(baseurl, x, query)
}
