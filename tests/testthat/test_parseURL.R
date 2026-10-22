library("testthat")
library("neotoma2")

context("Test `parseURL()` transport helpers.")

test_that("neotoma_baseurl resolves servers and honors APIPOINT", {
  expect_equal(neotoma2:::neotoma_baseurl("neotoma"),
               "https://api.neotomadb.org/v2.0/")
  expect_equal(neotoma2:::neotoma_baseurl("dev"),
               "https://api-dev.neotomadb.org/v2.0/")
  expect_equal(neotoma2:::neotoma_baseurl("local"),
               "http://localhost:3001/v2.0/")
  old <- Sys.getenv("APIPOINT")
  Sys.setenv("APIPOINT" = "local")
  expect_equal(neotoma2:::neotoma_baseurl("neotoma"),
               "http://localhost:3001/v2.0/")
  if (old == "") Sys.unsetenv("APIPOINT") else Sys.setenv("APIPOINT" = old)
})

test_that("neotoma_body builds a JSON body from the query", {
  body <- neotoma2:::neotoma_body(list(siteid = c(1, 2), limit = 50))
  expect_type(body, "character")
  expect_equal(as.character(body), "{\"siteid\":[1,2],\"limit\":50}")
})

## Pagination and retry. Every HTTP call below is stubbed, so these tests are
## pure logic: they touch no API and need no skip.

cz_geojson <- paste0("{\"type\":\"Polygon\",\"coordinates\":[[[12.4,50.14],",
                     "[14.1,48.64],[16.95,48.66],[18.91,49.61],",
                     "[15.24,50.99],[12.4,50.14]]]}")

api_url <- "https://api.neotomadb.org/v2.0/data/sites"

fake_response <- function(status, headers = list()) {
  structure(list(url = "https://api.neotomadb.org/v2.0/data/sites",
                 status_code = as.integer(status),
                 headers = headers,
                 all_headers = list(),
                 content = raw(0),
                 request = list(),
                 date = Sys.time(),
                 times = numeric(0)),
            class = "response")
}

## A stub for neotoma_fetch() that serves `total` records `limit` at a time and
## records the offset and limit of every request it receives.
fake_api <- function(total, log) {
  function(baseurl, x, query) {
    log$offsets <- c(log$offsets, query$offset)
    log$limits <- c(log$limits, query$limit)
    n <- min(query$limit, max(0, total - query$offset))
    list(status = 200,
         data = replicate(n, list(siteid = 1), simplify = FALSE),
         message = "Success")
  }
}

test_that("pagination stops on a short page instead of on an empty one", {
  # The API honours `limit` exactly, so a page shorter than `limit` is the last
  # page. Confirming that with a further request costs a full round trip -- on
  # the spatial endpoints, tens of seconds -- and returns nothing.
  log <- new.env()
  local_mocked_bindings(neotoma_fetch = fake_api(185, log),
                        .package = "neotoma2")

  result <- neotoma2:::neotoma_paginate("https://api.neotomadb.org/v2.0/",
                                        "data/sites", list(loc = cz_geojson))

  expect_length(result$data, 185)
  expect_equal(log$offsets, 0)
})

test_that("pagination walks every page of a multi-page result", {
  log <- new.env()
  local_mocked_bindings(neotoma_fetch = fake_api(4500, log),
                        .package = "neotoma2")

  result <- neotoma2:::neotoma_paginate("https://api.neotomadb.org/v2.0/",
                                        "data/sites", list(loc = cz_geojson))

  expect_length(result$data, 4500)
  expect_equal(log$offsets, c(0, 2000, 4000))
})

test_that("a result that is an exact multiple of the page size is complete", {
  # The one case where the extra request is unavoidable: a full final page is
  # indistinguishable from a page with more behind it.
  log <- new.env()
  local_mocked_bindings(neotoma_fetch = fake_api(4000, log),
                        .package = "neotoma2")

  result <- neotoma2:::neotoma_paginate("https://api.neotomadb.org/v2.0/",
                                        "data/sites", list(siteid = 1))

  expect_length(result$data, 4000)
  expect_equal(log$offsets, c(0, 2000, 4000))
})

test_that("an empty result returns no data and makes one request", {
  log <- new.env()
  local_mocked_bindings(neotoma_fetch = fake_api(0, log),
                        .package = "neotoma2")

  result <- neotoma2:::neotoma_paginate("https://api.neotomadb.org/v2.0/",
                                        "data/sites", list(loc = cz_geojson))

  expect_length(result$data, 0)
  expect_equal(log$offsets, 0)
})

test_that("pages are requested at the largest size the API supports", {
  # The API's limit and offset do not compose: walking a query in smaller pages
  # returns fewer records overall, because the second page comes back short.
  # Shrinking the page size therefore loses data, and is not a speed knob.
  log <- new.env()
  local_mocked_bindings(neotoma_fetch = fake_api(10, log),
                        .package = "neotoma2")

  neotoma2:::neotoma_paginate("https://api.neotomadb.org/v2.0/", "data/sites",
                              list(loc = cz_geojson))

  expect_equal(log$limits, 2000)
})

test_that("the loc geometry is converted once, not once per page", {
  log <- new.env()
  calls <- 0
  local_mocked_bindings(
    neotoma_fetch = fake_api(4500, log),
    parseLocation = function(x) {
      calls <<- calls + 1
      list(structure(x, class = "json"))
    },
    .package = "neotoma2"
  )

  neotoma2:::neotoma_paginate("https://api.neotomadb.org/v2.0/",
                              "data/sites", list(loc = cz_geojson))

  expect_equal(length(log$offsets), 3)
  expect_equal(calls, 1)
})

test_that("an already-converted loc is recognised and passed through", {
  parsed <- neotoma2:::parseLocation(cz_geojson)

  expect_false(neotoma2:::is_parsed_location(cz_geojson))
  expect_true(neotoma2:::is_parsed_location(parsed))
  expect_identical(neotoma2:::neotoma_location(parsed), parsed)

  from_parsed <- neotoma2:::neotoma_body(list(loc = parsed))
  from_raw <- neotoma2:::neotoma_body(list(loc = cz_geojson))
  expect_identical(as.character(from_parsed), as.character(from_raw))
})

test_that("a timed-out request is retried at most once", {
  # The API keeps working on a query we have abandoned, so retrying a timeout
  # stacks concurrent load onto an endpoint that is already struggling.
  attempts <- 0
  local_mocked_bindings(VERB = function(...) {
    attempts <<- attempts + 1
    stop("Timeout was reached: Operation timed out after 180000 ms")
  }, .package = "neotoma2")

  expect_error(neotoma2:::neotoma_retry("POST", api_url, times = 4),
               "did not respond within")
  expect_equal(attempts, 2)
})

test_that("rate limiting (429) is retried up to `times`", {
  attempts <- 0
  local_mocked_bindings(VERB = function(...) {
    attempts <<- attempts + 1
    fake_response(429)
  }, neotoma_wait = function(response, attempt) invisible(0),
  .package = "neotoma2")

  response <- neotoma2:::neotoma_retry("GET", api_url, times = 3)

  expect_equal(attempts, 3)
  expect_equal(response$status_code, 429)
})

test_that("a busy gateway (502/503/504) is retried at most once", {
  # The API keeps working on a query the gateway gave up on, so a second request
  # stacks a second heavy query on the first.
  for (status in c(502, 503, 504)) {
    attempts <- 0
    local_mocked_bindings(VERB = function(...) {
      attempts <<- attempts + 1
      fake_response(status)
    }, neotoma_wait = function(response, attempt) invisible(0),
    .package = "neotoma2")

    response <- neotoma2:::neotoma_retry("GET", api_url, times = 4)

    expect_equal(attempts, 2, info = paste("status", status))
    expect_equal(response$status_code, status)
  }
})

test_that("a deterministic server error (500) is not retried", {
  # Four identical requests for a 500 that can never succeed was the pattern
  # seen in the API logs.
  attempts <- 0
  local_mocked_bindings(VERB = function(...) {
    attempts <<- attempts + 1
    fake_response(500)
  }, neotoma_wait = function(response, attempt) {
    stop("must not wait for a 500")
  }, .package = "neotoma2")

  response <- neotoma2:::neotoma_retry("POST", api_url, times = 4)

  expect_equal(attempts, 1)
  expect_equal(response$status_code, 500)
})

test_that("the wait before a retry follows the policy", {
  slept <- NULL
  local_mocked_bindings(neotoma_sleep = function(seconds) {
    slept <<- c(slept, seconds)
  }, .package = "neotoma2")

  # A busy gateway waits 30 to 60 s, with jitter.
  busy <- fake_response(503)
  suppressMessages(replicate(20, neotoma2:::neotoma_wait(busy, 1)))
  expect_true(all(slept >= 30 & slept <= 60))
  expect_gt(length(unique(slept)), 1)

  # Retry-After is honoured as sent, and capped at two minutes.
  slept <- NULL
  neotoma2:::neotoma_wait(fake_response(429, list("retry-after" = "7")), 1)
  too_long <- fake_response(429, list("retry-after" = "9999"))
  suppressMessages(neotoma2:::neotoma_wait(too_long, 1))
  expect_equal(slept, c(7, 120))

  # A Retry-After that is a date, or nonsense, is ignored; so is a connection
  # error. Both fall back to a short jittered backoff.
  slept <- NULL
  a_date <- fake_response(429,
                          list("retry-after" = "Wed, 21 Oct 2026 07:28:00 GMT"))
  neotoma2:::neotoma_wait(a_date, 2)
  neotoma2:::neotoma_wait(simpleError("Could not resolve host"), 2)
  expect_true(all(slept >= 3 & slept <= 5))
})

test_that("the default timeout sits just above the gateway cut-off", {
  old <- Sys.getenv("NEOTOMA_TIMEOUT", unset = NA)
  Sys.unsetenv("NEOTOMA_TIMEOUT")
  on.exit(if (!is.na(old)) Sys.setenv(NEOTOMA_TIMEOUT = old))
  expect_equal(formals(neotoma2:::neotoma_retry)$seconds[[3]], 125)
  Sys.setenv(NEOTOMA_TIMEOUT = "30")
  expect_equal(neotoma2:::neotoma_env_num("NEOTOMA_TIMEOUT", 125), 30)
})

test_that("client errors are not retried", {
  attempts <- 0
  local_mocked_bindings(VERB = function(...) {
    attempts <<- attempts + 1
    fake_response(404)
  }, .package = "neotoma2")

  response <- neotoma2:::neotoma_retry("GET", api_url, times = 4)

  expect_equal(attempts, 1)
  expect_equal(response$status_code, 404)
})

test_that("a successful request is made exactly once", {
  attempts <- 0
  local_mocked_bindings(VERB = function(...) {
    attempts <<- attempts + 1
    fake_response(200)
  }, .package = "neotoma2")

  neotoma2:::neotoma_retry("GET", api_url, times = 4)

  expect_equal(attempts, 1)
})

test_that("NEOTOMA_RETRIES caps attempts so test runs fail fast", {
  old <- Sys.getenv("NEOTOMA_RETRIES")
  Sys.setenv("NEOTOMA_RETRIES" = "1")
  on.exit(if (old == "") {
    Sys.unsetenv("NEOTOMA_RETRIES")
  } else {
    Sys.setenv("NEOTOMA_RETRIES" = old)
  })

  attempts <- 0
  local_mocked_bindings(VERB = function(...) {
    attempts <<- attempts + 1
    stop("Timeout was reached")
  }, .package = "neotoma2")

  expect_error(neotoma2:::neotoma_retry("GET", api_url))
  expect_equal(attempts, 1)
})

test_that("get_params fetches the Swagger document once per session", {
  cache <- neotoma2:::swagger_cache
  rm(list = ls(cache), envir = cache)

  fetches <- 0
  local_mocked_bindings(get_swagger = function() {
    fetches <<- fetches + 1
    paste0('"v2.0/data/testendpoint": {',
           '"#/components/parameters/siteidQuery"',
           '"#/components/parameters/sitenameQuery"', "}\n")
  }, .package = "neotoma2")

  first <- neotoma2:::get_params("testendpoint")
  second <- neotoma2:::get_params("testendpoint")

  expect_equal(fetches, 1)
  expect_identical(first, second)
  expect_true(all(c("siteid", "sitename", "limit", "offset", "all_data") %in%
                    unlist(first)))
})


## Request identity, error messages, and the paths that send an id list.

fake_body <- function(text, status = 200) {
  r <- fake_response(status)
  r$content <- charToRaw(text)
  r
}

test_that("the User-Agent carries the package and R versions", {
  ua <- neotoma2:::neotoma_user_agent()
  expect_match(ua, "^neotoma2/[0-9][0-9.]* \\(R [0-9.]+\\)$")
  expect_match(ua, as.character(utils::packageVersion("neotoma2")),
               fixed = TRUE)
})

test_that("a contact is added to the User-Agent only when one is set", {
  old_opt <- options(neotoma2.contact = NULL)
  old_env <- Sys.getenv("NEOTOMA_CONTACT", unset = NA)
  Sys.unsetenv("NEOTOMA_CONTACT")
  on.exit({
    options(old_opt)
    if (!is.na(old_env)) Sys.setenv(NEOTOMA_CONTACT = old_env)
  })
  expect_false(grepl("contact", neotoma2:::neotoma_user_agent()))

  Sys.setenv(NEOTOMA_CONTACT = "env@example.org")
  expect_match(neotoma2:::neotoma_user_agent(),
               "; contact: env@example.org)", fixed = TRUE)

  options(neotoma2.contact = "opt@example.org")
  expect_match(neotoma2:::neotoma_user_agent(),
               "; contact: opt@example.org)", fixed = TRUE)
})

test_that("a web page in place of data gets a clear error", {
  # The API's catch-all redirects an unknown route to its documentation.
  html <- fake_body("<!DOCTYPE html><html><body>api-docs</body></html>")
  expect_error(neotoma2:::neotoma_content(html), "web page instead of data")
  ok <- fake_body('{"status":"success"}')
  expect_equal(neotoma2:::neotoma_content(ok)$status, "success")
})

json_failure <- function(status, json) {
  r <- fake_response(status)
  r$content <- charToRaw(json)
  r$headers <- list("content-type" = "application/json")
  r
}

test_that("the API's reason for a failure is shown to the user", {
  # Current shape: the database error is in `message`, `data` is null.
  json <- paste0('{"status":"failure","data":null,',
                 '"message":"operator does not exist: integer = text"}')
  new_shape <- json_failure(500, json)
  expect_equal(neotoma2:::neotoma_api_message(new_shape),
               "operator does not exist: integer = text")
  expect_error(neotoma2:::neotoma_status_check(new_shape),
               "The API said: operator does not exist: integer = text")
  # Earlier shape: the database error is in `data`, `message` is generic.
  json <- paste0('{"status":"failure","data":',
                 '"operator does not exist: integer = text",',
                 '"message":"SQL Error."}')
  old_shape <- json_failure(500, json)
  expect_equal(neotoma2:::neotoma_api_message(old_shape),
               "operator does not exist: integer = text")
  # No usable reason: the plain HTTP error still comes through.
  html <- fake_response(500)
  html$content <- charToRaw("<html>oops</html>")
  expect_true(is.na(neotoma2:::neotoma_api_message(html)))
  expect_error(neotoma2:::neotoma_status_check(html), "500")
  expect_true(is.na(neotoma2:::neotoma_api_message(fake_response(500))))
})

test_that("a busy server is named as such, not blamed on the API path", {
  expect_error(neotoma2:::neotoma_status_check(fake_response(504)),
               "busy or restarting")
  expect_error(neotoma2:::neotoma_status_check(fake_response(429)),
               "Too many requests")
})

## Capture the requests that neotoma_retry() would make, and answer each with
## `data` records that name the ids found in the request URL.
record_requests <- function(log) {
  function(verb, url, ...) {
    args <- list(...)
    log$verb <- c(log$verb, verb)
    log$url <- c(log$url, url)
    log$body <- c(log$body, if (!is.null(args$body)) as.character(args$body))
    log$config <- c(log$config, list(Filter(function(a) inherits(a, "request"),
                                            args)))
    ids <- if (!is.null(args$body)) {
      sent <- jsonlite::fromJSON(as.character(args$body))
      as.character(if (!is.null(sent$siteid)) sent$siteid else sent$datasetid)
    } else {
      regmatches(url, regexpr("[0-9][0-9,]*$", url))
    }
    recs <- if (length(ids)) {
      lapply(strsplit(ids, ",")[[1]], function(i) list(id = as.numeric(i)))
    } else {
      list()
    }
    body <- list(status = "success", data = recs)
    fake_body(as.character(jsonlite::toJSON(body, auto_unbox = TRUE)))
  }
}

test_that("long id lists go as short GETs where there is no POST route", {
  # Taxa, publications and contacts have no POST route: a POST to them is
  # redirected to /api-docs.
  for (endpoint in c("data/taxa/", "data/publications/", "data/contacts/")) {
    log <- new.env()
    local_mocked_bindings(neotoma_retry = record_requests(log),
                          .package = "neotoma2")
    path <- paste0(endpoint, paste(1:1500, collapse = ","))
    result <- neotoma2:::neotoma_fetch("https://api.neotomadb.org/", path,
                                       list(offset = 0, limit = 2000))
    expect_true(all(log$verb == "GET"), info = endpoint)
    expect_length(log$url, 15)
    expect_true(all(nchar(log$url) < 2000), info = endpoint)
    # every id is requested exactly once, in order, and the records come back
    # combined in that order
    requested <- as.numeric(unlist(strsplit(sub(".*[=/]", "", log$url), ",")))
    expect_equal(requested, 1:1500, info = endpoint)
    expect_equal(vapply(result$data, function(r) r$id, numeric(1)), 1:1500,
                 info = endpoint)
  }
})

test_that("long id lists for sites, datasets, downloads POST in chunks", {
  cases <- list(
    list(path = "data/sites/", post = "data/sites", key = "siteid"),
    list(path = "data/datasets/", post = "data/datasets", key = "datasetid"),
    list(path = "data/downloads?datasetid=", post = "data/downloads",
         key = "datasetid")
  )
  for (case in cases) {
    log <- new.env()
    local_mocked_bindings(neotoma_retry = record_requests(log),
                          .package = "neotoma2")
    path <- paste0(case$path, paste(1:1500, collapse = ","))
    result <- neotoma2:::neotoma_fetch("https://api.neotomadb.org/v2.0/", path,
                                       list(offset = 0, limit = 2000))
    expect_true(all(log$verb == "POST"), info = case$path)
    expect_length(log$url, 15)
    # the ids are in the body, never in the URL, so no `?datasetid=` prefix can
    # leak into a body value
    expect_true(all(log$url == paste0("https://api.neotomadb.org/v2.0/",
                                      case$post)), info = case$path)
    bodies <- lapply(log$body, jsonlite::fromJSON)
    expect_true(all(vapply(bodies, function(b) !is.null(b[[case$key]]),
                           logical(1))), info = case$path)
    expect_true(all(vapply(bodies, function(b) b$limit == 2000, logical(1))),
                info = case$path)
    sent <- as.numeric(unlist(strsplit(vapply(bodies, function(b) b[[case$key]],
                                              character(1)), ",")))
    expect_equal(sent, 1:1500, info = case$path)
    expect_equal(vapply(result$data, function(r) r$id, numeric(1)), 1:1500,
                 info = case$path)
  }
})

test_that("a POST keeps its method and body through a redirect", {
  # libcurl turns a redirected POST into a GET and drops the body unless asked
  # not to; the API then answers with its unfiltered first page.
  log <- new.env()
  local_mocked_bindings(neotoma_retry = record_requests(log),
                        .package = "neotoma2")
  neotoma2:::neotoma_post("https://api.neotomadb.org/v2.0/data/sites",
                          list(siteid = "24,25"))
  expect_true(any(vapply(log$config[[1]],
                         function(a) identical(a$options$postredir, 7),
                         logical(1))))
})

test_that("a POST for ids that were not applied is reported, not returned", {
  # A server that ignores the body returns its default page of records.
  default_page <- function(verb, url, ...) {
    recs <- lapply(1:25, function(i) list(siteid = i))
    body <- list(status = "success", data = recs)
    fake_body(as.character(jsonlite::toJSON(body, auto_unbox = TRUE)))
  }
  local_mocked_bindings(neotoma_retry = default_page, .package = "neotoma2")
  # more records than ids
  api <- "https://api.neotomadb.org/v2.0/"
  expect_error(neotoma2:::neotoma_get_ids(api, "data/sites/24,25,26", list()),
               "ids were not applied")
  # fewer records than ids, but not the ones asked for
  other_ids <- paste0("data/sites/", paste(101:200, collapse = ","))
  expect_error(neotoma2:::neotoma_get_ids(api, other_ids, list()),
               "ids were not applied")
  returned <- list(list(siteid = 24), list(siteid = 25))
  expect_true(neotoma2:::neotoma_ids_applied(returned, "siteid",
                                             c("24", "25", "26")))
})

test_that("a spatial loc query is still sent as a POST body", {
  log <- new.env()
  local_mocked_bindings(neotoma_retry = record_requests(log),
                        .package = "neotoma2")
  neotoma2:::neotoma_fetch("https://api.neotomadb.org/", "data/datasets",
                           list(loc = cz_geojson, limit = 5))
  expect_equal(log$verb, "POST")
  expect_equal(log$url, "https://api.neotomadb.org/data/datasets")
  expect_true(jsonlite::validate(log$body))
})

test_that("a long filter list is POSTed where there is a route, else one GET", {
  log <- new.env()
  local_mocked_bindings(neotoma_retry = record_requests(log),
                        .package = "neotoma2")
  neotoma2:::parseURL("data/datasets", taxa = paste0("Taxon", 1:300), limit = 5)
  expect_equal(log$verb, "POST")
  expect_equal(jsonlite::fromJSON(log$body)$limit, 5)
  expect_length(strsplit(jsonlite::fromJSON(log$body)$taxa, ",")[[1]], 300)

  log2 <- new.env()
  local_mocked_bindings(neotoma_retry = record_requests(log2),
                        .package = "neotoma2")
  neotoma2:::parseURL("data/publications", search = strrep("word ", 500),
                      limit = 5)
  expect_equal(log2$verb, "GET")
  expect_length(log2$url, 1)
})

test_that("only the endpoints the API accepts a POST for are POST-routed", {
  yes <- c("data/sites", "data/datasets", "data/downloads", "data/datasets/24",
           "data/sites/24/datasets", "data/geopoliticalunits/1/sites",
           "data/downloads?datasetid=1,2")
  no <- c("data/taxa", "data/taxa/1,2", "data/publications", "data/contacts",
          "data/sites/24", "data/sites/24/chronologies",
          "data/dbtables/agetypes")
  expect_true(all(vapply(yes, neotoma2:::neotoma_post_route, logical(1))))
  expect_false(any(vapply(no, neotoma2:::neotoma_post_route, logical(1))))
})

test_that("a short id list is still a single GET", {
  log <- new.env()
  local_mocked_bindings(neotoma_retry = record_requests(log),
                        .package = "neotoma2")
  neotoma2:::neotoma_fetch("https://api.neotomadb.org/", "data/taxa/1,2,3",
                           list())
  expect_equal(log$verb, "GET")
  expect_length(log$url, 1)
})

test_that("a multi-valued argument is sent as one comma-separated value", {
  # httr/modify_url take one value per parameter; vectors used to stop the
  # request with "values must be length 1".
  sent <- NULL
  local_mocked_bindings(neotoma_fetch = function(baseurl, x, query) {
    sent <<- query
    list(status = 200, data = list())
  }, .package = "neotoma2")
  neotoma2:::parseURL("data/datasets", taxa = c("Abies", "Pinus"),
                      datasettype = c("pollen", "macrofossil"), limit = 5,
                      loc = c(-80, 40, -70, 50))
  expect_equal(sent$taxa, "Abies,Pinus")
  expect_equal(sent$datasettype, "pollen,macrofossil")
  expect_equal(sent$limit, 5)
  expect_equal(sent$loc, c(-80, 40, -70, 50))
})

test_that("a vector parameter makes a valid GET URL end to end", {
  log <- new.env()
  local_mocked_bindings(neotoma_retry = function(verb, url, ..., query = NULL) {
    log$query <- query
    fake_body('{"status":"success","data":[]}')
  }, .package = "neotoma2")
  expect_no_error(neotoma2:::parseURL("data/datasets",
                                      taxa = c("Abies", "Pinus")))
  expect_equal(log$query$taxa, "Abies,Pinus")
})
