library("testthat")
library("neotoma2")

context("`taxa()` function")
test_that("taxa() returns only unique results", {
  skip_on_cran()
  # Live: this two-site download is ~0.8 MB, too large to ship as a fixture, so
  # it stays live and skips on CI (GitHub Actions).
  skip_on_ci()
  mydataset <- get_downloads(c(1435, 24238))
  df <- taxa(mydataset)
  testthat::expect_false(any(duplicated(df)))
  testthat::expect_is(df, "data.frame")
})