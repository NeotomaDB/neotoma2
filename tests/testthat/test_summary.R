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
