library("testthat")
library("neotoma2")

context("Test `parseURL()` transport helpers.")

test_that("neotoma_baseurl resolves servers and honors APIPOINT", {
  expect_equal(neotoma2:::neotoma_baseurl("neotoma"),
               "https://api.neotomadb.org/v2.0/")
  expect_equal(neotoma2:::neotoma_baseurl("dev"),
               "http://api-dev.neotomadb.org/v2.0/")
  expect_equal(neotoma2:::neotoma_baseurl("local"),
               "http://localhost:3001/v2.0/")
  old <- Sys.getenv("APIPOINT")
  Sys.setenv("APIPOINT" = "local")
  expect_equal(neotoma2:::neotoma_baseurl("neotoma"),
               "http://localhost:3001/v2.0/")
  if (old == "") Sys.unsetenv("APIPOINT") else Sys.setenv("APIPOINT" = old)
})

test_that("neotoma_id_param names the identifier from the endpoint path", {
  expect_equal(neotoma2:::neotoma_id_param("data/sites/1,2,3"), "siteid")
  expect_equal(neotoma2:::neotoma_id_param("data/datasets/1,2"), "datasetid")
  expect_equal(neotoma2:::neotoma_id_param("data/downloads?datasetid=5"),
               "datasetid")
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

fake_response <- function(status) {
  structure(list(url = "https://api.neotomadb.org/v2.0/data/sites",
                 status_code = as.integer(status),
                 headers = list(),
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

test_that("transient server failures are retried up to `times`", {
  attempts <- 0
  local_mocked_bindings(VERB = function(...) {
    attempts <<- attempts + 1
    fake_response(502)
  }, .package = "neotoma2")

  response <- neotoma2:::neotoma_retry("GET", api_url, times = 3)

  expect_equal(attempts, 3)
  expect_equal(response$status_code, 502)
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
