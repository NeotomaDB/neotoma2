library("testthat")
library("neotoma2")

context("`get_taxa()` and `get_taxon()` functions")
test_that("get_taxa() sites object", {
  skip_on_cran()
  # get_taxa(1) returns every site with this taxon, then downloads all of them:
  # a very large, flap-prone query. Skip on CI (GitHub Actions), and skip
  # locally when the API is unreachable.
  skip_on_ci()
  skip_if_api_unreachable()
  abies <- get_taxa(1)
  testthat::expect_is(abies, "sites")
  abies_df <- abies %>% get_downloads() %>% taxa() %>% suppressWarnings()
  testthat::expect_true(any(grepl("Abies", abies_df$variablename)))
  abies <- get_taxa(taxonname = "abies")
  testthat::expect_is(abies, "sites")
  abies_df <- abies %>% get_downloads() %>% taxa() %>% suppressWarnings()
  testthat::expect_true(any(grepl("Abies", abies_df$variablename)))
})

## Offline tests. `get_taxon()` and `get_datasets()` are replaced, so these
## touch no API and need no skip. They check the requests `get_taxa()` makes.

fake_taxon <- function(names) {
  force(names)
  function(...) {
    if (is.null(names)) return(NULL)
    new("taxa", taxa = lapply(seq_along(names), function(i) {
      new("taxon", taxonid = i, taxonname = names[[i]])
    }))
  }
}

# Replace get_taxon()/get_datasets(); `log` records what get_datasets() gets.
mock_taxa_steps <- function(names, log, .env = parent.frame()) {
  record_datasets <- function(...) {
    log$calls <- c(log$calls, list(list(...)))
    "datasets"
  }
  local_mocked_bindings(get_taxon = fake_taxon(names),
                        get_datasets = record_datasets,
                        .package = "neotoma2", .env = .env)
}

test_that("get_taxa(taxa=) sends the names, not the taxa() generic", {
  log <- new.env()
  mock_taxa_steps("Abies", log)
  expect_equal(get_taxa(taxa = "Pinus"), "datasets")
  expect_equal(log$calls[[1]]$taxa, "Pinus")
  expect_type(log$calls[[1]]$taxa, "character")
})

test_that("get_taxa honours a caller's all_data in both calls", {
  log <- new.env()
  mock_taxa_steps("Abies", log)
  get_taxa(taxonname = "Abies", all_data = TRUE)
  get_taxa(taxa = "Abies", all_data = TRUE)
  expect_true(log$calls[[1]]$all_data)
  expect_true(log$calls[[2]]$all_data)
})

test_that("get_taxa(taxonname=) forwards the dataset arguments", {
  log <- new.env()
  mock_taxa_steps("Pinus", log)
  get_taxa(taxonname = "Pinus", datasettype = c("pollen", "macrofossil"),
           loc = c(-80, 40, -70, 50), limit = 5, offset = 10,
           ageyoung = 100, ageold = 5000)
  call <- log$calls[[1]]
  expect_equal(call$taxa, "Pinus")
  expect_equal(call$datasettype, c("pollen", "macrofossil"))
  expect_equal(call$loc, c(-80, 40, -70, 50))
  expect_equal(call[c("limit", "offset", "ageyoung", "ageold")],
               list(limit = 5, offset = 10, ageyoung = 100, ageold = 5000))
  expect_false("taxonname" %in% names(call))
})

test_that("get_taxa(<id>) filters the datasets by the taxon's names", {
  # The numeric method used to call get_datasets() with no taxa filter at all.
  log <- new.env()
  mock_taxa_steps(c("Abies", "Abies alba"), log)
  expect_equal(get_taxa(1, limit = 5), "datasets")
  expect_equal(log$calls[[1]]$taxa, c("Abies", "Abies alba"))
  expect_equal(log$calls[[1]]$limit, 5)
})

test_that("get_taxa sends only the id to the taxa endpoint", {
  seen <- NULL
  fake_get_taxon <- function(...) {
    seen <<- list(...)
    new("taxa", taxa = list(new("taxon", taxonid = 1, taxonname = "Abies")))
  }
  local_mocked_bindings(get_taxon = fake_get_taxon,
                        get_datasets = function(...) "datasets",
                        .package = "neotoma2")
  get_taxa(1, limit = 5, offset = 2, all_data = TRUE, loc = c(1, 2, 3, 4))
  expect_equal(seen, list(1))
})

test_that("get_taxa never asks for datasets without a taxon filter", {
  log <- new.env()
  mock_taxa_steps(NULL, log)
  expect_message(res <- get_taxa(taxonname = "Zzzzz not a taxon"),
                 "No datasets were requested")
  expect_null(res)
  expect_message(res <- get_taxa(999999999), "No datasets were requested")
  expect_null(res)
  expect_message(res <- get_taxa(taxa = character(0)),
                 "No datasets were requested")
  expect_null(log$calls)
})

test_that("get_taxa asks for a taxon", {
  expect_error(get_taxa(limit = 5), "taxon ID")
})

test_that("get_taxa warns before sending a very broad taxa filter", {
  log <- new.env()
  mock_taxa_steps(paste("Taxon", 1:80), log)
  expect_warning(get_taxa(taxonname = "Taxon%"), "80 taxon names")
  expect_length(log$calls[[1]]$taxa, 80)

  mock_taxa_steps(paste("Taxon", 1:25), log)
  expect_no_warning(get_taxa(taxonname = "Taxon%"))
})

test_that("get_taxa(<taxonid>) searches datasets for that taxon, not all", {
  # A user looked up `Bison*`, took the second record (Bison bison, 5868) and
  # passed its id to get_taxa(). The datasets that came back were pollen surface
  # samples with none of the taxon: the old get_taxa(<id>) called get_datasets()
  # with no taxon filter, so it returned the API's first page of datasets.
  bison_taxa <- list(new("taxon", taxonid = 6774, taxonname = "Bison"),
                     new("taxon", taxonid = 5868, taxonname = "Bison bison"),
                     new("taxon", taxonid = 6772, taxonname = "Bison priscus"))
  bison <- new("taxa", taxa = bison_taxa)
  seen <- new.env()
  fake_get_taxon <- function(x = NULL, ...) {
    seen$taxon_args <- c(seen$taxon_args, list(list(x, ...)))
    if (is.numeric(x)) {
      new("taxa", taxa = Filter(function(t) t@taxonid %in% x, bison@taxa))
    } else {
      bison
    }
  }
  fake_get_datasets <- function(...) {
    seen$datasets <- c(seen$datasets, list(list(...)))
    "datasets"
  }
  local_mocked_bindings(get_taxon = fake_get_taxon,
                        get_datasets = fake_get_datasets,
                        .package = "neotoma2")

  second <- get_taxon(taxonname = "Bison*")[2]
  expect_equal(second$taxonid, 5868)
  expect_equal(get_taxa(second$taxonid, all_data = TRUE), "datasets")

  # the taxon id is looked up on its own: nothing for the datasets request
  # leaks into the taxa endpoint
  expect_equal(seen$taxon_args[[2]], list(5868))
  # exactly one datasets request, filtered by that taxon's name and by nothing
  # else the caller did not ask for
  expect_length(seen$datasets, 1)
  expect_equal(seen$datasets[[1]], list(taxa = "Bison bison", all_data = TRUE))
})

test_that("get_taxa(<taxonid>) asks for no datasets when the id is unknown", {
  log <- new.env()
  mock_taxa_steps(NULL, log)
  expect_message(expect_null(get_taxa(999999999)), "No taxon found")
  expect_null(log$calls)
})
