library("testthat")
library("neotoma2")
library("dplyr")
library("httptest")

context("Chronology controls functions work as expected.")
httptest::with_mock_api({
test_that("Chroncontrols gets record", {
  skip_on_cran()
  single <- chroncontrols(get_downloads(4716))
  multi <- chroncontrols(get_downloads(21007))
  mamchron <- chroncontrols(get_downloads(4564))
  
  testthat::expect_is(single, "data.frame")
  testthat::expect_equal(length(unique(single$chronologyid)), 3)
  testthat::expect_is(multi, "data.frame")
  testthat::expect_gt(length(unique(multi$chronologyid)), 4)
  testthat::expect_is(mamchron, "data.frame")
})
})

context("Chronology controls return nothing when nothing.")
httptest::with_mock_api({
test_that("Chroncontrols is empty for a record with no chronology", {
  skip_on_cran()
  empty <- chroncontrols(get_downloads(100))

  testthat::expect_s3_class(empty, "data.frame")
  testthat::expect_equal(nrow(empty), 0)
})
})
