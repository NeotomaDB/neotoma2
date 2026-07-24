library("testthat")
library("neotoma2")
library("httptest")

httptest::with_mock_api({
test_that("collunits()`", {
  skip_on_cran()
  # Bounded to a few known sites so the recorded fixture stays small; the
  # accessor assertions below hold for any handful of sites.
  dls <- get_sites(c(24, 1001, 2001)) %>%
    get_downloads()
  sumDl <- summary(dls)
  ids <- getids(dls)
  cus <- collunits(dls)
  testthat::expect_identical(length(unique(cus$collectionunitid)),
                             length(unique(ids$collunitid)))
  testthat::expect_true(all(sumDl$collunit_name %in% cus$handle))
})
})
