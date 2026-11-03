library("testthat")
library("neotoma2")

# Live smoke tests: a handful of small, fast requests that show whether the
# server selected by `APIPOINT` (production by default) answers and returns
# well-formed records. They are deliberately light and not mocked, so the
# scheduled `live-api` workflow can run them against production without adding
# noticeable load. They skip on CRAN like every other live test.

context("Live smoke tests against the selected Neotoma API")

test_that("get_sites() returns the sites asked for", {
  skip_on_cran()
  sites <- get_sites(limit = 3)
  testthat::expect_s4_class(sites, "sites")
  testthat::expect_length(sites, 3)
})

test_that("get_datasets() returns datasets", {
  skip_on_cran()
  ds <- get_datasets(limit = 3)
  testthat::expect_s4_class(ds, "sites")
  testthat::expect_gt(nrow(getids(ds)), 0)
})

test_that("get_downloads() and samples() work for one dataset", {
  skip_on_cran()
  dl <- get_downloads(4716)
  testthat::expect_s4_class(dl, "sites")
  smp <- samples(dl)
  testthat::expect_s3_class(smp, "data.frame")
  testthat::expect_gt(nrow(smp), 0)
})

test_that("chronologies() ties chronologies to datasets", {
  skip_on_cran()
  chron <- as.data.frame(chronologies(get_downloads(4716)))
  idcols <- c("siteid", "collunitid", "datasetid")
  testthat::expect_true(all(idcols %in% names(chron)))
  testthat::expect_true(all(chron$datasetid == 4716))
})

test_that("get_taxon() honours limit", {
  skip_on_cran()
  taxa <- get_taxon(taxonname = "Pinus%", limit = 3)
  testthat::expect_s4_class(taxa, "taxa")
  testthat::expect_length(taxa, 3)
})

test_that("get_table() returns a table", {
  skip_on_cran()
  tb <- get_table("agetypes", limit = 2, refresh = TRUE)
  testthat::expect_s3_class(tb, "data.frame")
  testthat::expect_equal(nrow(tb), 2)
})
