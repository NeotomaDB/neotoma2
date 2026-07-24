library("testthat")
library("neotoma2")
library("httptest")

httptest::with_mock_api({
test_that("`datasets()`", {
  skip_on_cran()
  # Bounded to a few known sites so the recorded fixture stays small; the
  # accessor invariants below hold for any handful of sites.
  dls <- get_sites(c(24, 1001, 2001)) %>%
    get_downloads()
  sumDl <- summary(dls)
  ids <- getids(dls)
  dss <- datasets(dls)
  testthat::expect_identical(length(dss), nrow(ids))
  testthat::expect_identical(sum(sumDl$n_datasets), nrow(ids))
  testthat::expect_identical(length(unique(dss$datasetid)),
                             length(unique(ids$datasetid)))
})
})
