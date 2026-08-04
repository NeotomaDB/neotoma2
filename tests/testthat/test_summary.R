library("testthat")
library("neotoma2")
library("httptest")

httptest::with_mock_api({
test_that("`datasets()`", {
  skip_on_cran()
  # Bounded to a few known sites so the recorded fixture stays small; the
  # assertions below only need a handful of sites/collunits/datasets to hold.
  dls <- get_sites(c(24, 1001, 2001)) %>%
    get_downloads()
  sumDl <- summary(dls)
  ids <- getids(dls)
  dss <- datasets(dls)
  cus <- collunits(dls)
  testthat::expect_identical(sum(sumDl$n_datasets), nrow(ids))
  testthat::expect_identical(length(unique(sumDl$siteid)), length(dls))
  testthat::expect_true(all(sumDl$collunit_name %in% cus$handle))
  testthat::expect_true(all(cus$handle %in% sumDl$collunit_name))
})
})

## A site does not have to carry datasets. `get_sites()` returns collection
## units before any dataset metadata is attached, and the `collunits` slot is
## optional, so summary() has to survive both. It used to drop the site's only
## row on the way through na.omit() and then fail assembling a data.frame from
## a 1-row site and a 0-row collunit table.
test_that("summary() reports one row per site when there are no datasets", {
  bare <- set_site(siteid = 1, sitename = "No collection units")

  with_cu <- set_site(siteid = 2, sitename = "Collection unit, no datasets")
  with_cu@collunits <- methods::new("collunits",
                                    collunits = list(set_collunit(handle = "CU1")))

  for (s in list(bare, with_cu)) {
    result <- summary(methods::new("sites", sites = list(s)))
    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_equal(nrow(result), 1)
    testthat::expect_equal(result$n_datasets, 0)
  }

  both <- summary(methods::new("sites", sites = list(bare, with_cu)))
  testthat::expect_equal(nrow(both), 2)
  testthat::expect_equal(both$siteid, c(1, 2))
})
