library("testthat")
library("neotoma2")

test_that("`get_taxon` yields specific taxon information", {
  skip_on_cran()
  abies_num <- get_taxon(1)
  testthat::expect_is(abies_num, "taxa")
  testthat::expect_is(abies_num[[1]], "taxon")

  abies_name <- get_taxon(taxonname = "abies")
  testthat::expect_is(abies_name, "taxa")
  testthat::expect_is(abies_name[[1]], "taxon")
  testthat::expect_equal(as.data.frame(abies_num), as.data.frame(abies_name))
})

## Offline tests: parseURL() is replaced, so these touch no API.

taxon_result <- function(n) {
  list(
    status = "success", message = "ok",
    data = lapply(seq_len(n), function(i) {
      list(taxonid = i, taxonname = paste("Taxon", i))
    })
  )
}

taxon_params <- function(...) list("taxonname", "limit", "offset", "all_data")

test_that("get_taxon(taxonname=) fetches every page by default", {
  # all_data was computed and never handed to parseURL(), so a wildcard came
  # back as the API's first page only.
  seen <- NULL
  fake_parse_url <- function(x, ...) {
    seen <<- list(x = x, ...)
    taxon_result(3)
  }
  local_mocked_bindings(
    get_params = taxon_params, parseURL = fake_parse_url,
    .package = "neotoma2"
  )
  out <- get_taxon(taxonname = "Pinus%")
  expect_equal(seen$x, "data/taxa")
  expect_true(seen$all_data)
  expect_equal(seen$taxonname, "Pinus%")
  expect_s4_class(out, "taxa")
  expect_length(out, 3)
})

test_that("get_taxon honours all_data = FALSE and never sends it twice", {
  seen <- NULL
  fake_parse_url <- function(x, ...) {
    seen <<- list(...)
    taxon_result(1)
  }
  local_mocked_bindings(
    get_params = taxon_params, parseURL = fake_parse_url,
    .package = "neotoma2"
  )
  get_taxon(taxonname = "Pinus%", all_data = FALSE)
  expect_false(seen$all_data)
  expect_equal(sum(names(seen) == "all_data"), 1)
})

test_that("a long vector of taxon ids never POSTs to data/taxa", {
  # The API has no POST route for taxa; the POST was redirected to /api-docs.
  verbs <- character(0)
  urls <- character(0)
  fake_retry <- function(verb, url, ...) {
    verbs <<- c(verbs, verb)
    urls <<- c(urls, url)
    ids <- strsplit(sub(".*/", "", url), ",")[[1]]
    records <- lapply(ids, function(i) {
      list(taxonid = as.numeric(i), taxonname = paste("Taxon", i))
    })
    body <- jsonlite::toJSON(list(status = "success", data = records),
      auto_unbox = TRUE
    )
    structure(
      list(
        url = url, status_code = 200L, headers = list(),
        content = charToRaw(as.character(body))
      ),
      class = "response"
    )
  }
  local_mocked_bindings(neotoma_retry = fake_retry, .package = "neotoma2")
  out <- get_taxon(1:1500)
  expect_true(all(verbs == "GET"))
  expect_length(urls, 15)
  expect_true(all(nchar(urls) < 2000))
  expect_s4_class(out, "taxa")
  expect_length(out, 1500)
})

test_that("get_taxon with no ids returns NULL and asks for nothing", {
  local_mocked_bindings(
    parseURL = function(...) stop("must not be called"),
    .package = "neotoma2"
  )
  expect_message(res <- get_taxon(numeric(0)), "No taxon ID")
  expect_null(res)
})


test_that("a taxon record without a taxon group still parses", {
  # `taxagroupid` is a character slot; a missing value used to be coerced to an
  # integer NA, which made the whole taxa object invalid.
  records <- list(
    list(taxonid = 1, taxonname = "Abies"),
    list(taxonid = 2, taxonname = "Pinus", taxagroupid = "VPL")
  )
  out <- neotoma2:::parse_taxon(list(data = records))
  expect_s4_class(out, "taxa")
  expect_true(is.na(out[[1]]@taxagroupid))
  expect_equal(out[[2]]@taxagroupid, "VPL")
})

test_that("a missing taxon id sends no request", {
  # `data/taxa/NA` makes the API fail with a database error.
  requests <- 0
  local_mocked_bindings(parseURL = function(...) {
    requests <<- requests + 1
    NULL
  }, .package = "neotoma2")
  expect_message(expect_null(get_taxon(NA_integer_)), "No taxon ID")
  expect_message(expect_null(get_taxon(c(NA_real_, NA_real_))), "No taxon ID")
  expect_equal(requests, 0)
})
