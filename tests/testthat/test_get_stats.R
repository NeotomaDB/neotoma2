library("testthat")
library("neotoma2")

context("`get_stats()` function returns different kinds of summary statistics")
test_that("get_stats with the default values", {
  skip_on_cran()
  stats <- get_stats()
  testthat::expect_is(stats, "data.frame")
  testthat::expect_true(ncol(stats) == 2)
  testthat::expect_true(all(c("databasename", "counts") %in% colnames(stats)))
})

test_that("get_stats rawbymonth", {
  skip_on_cran()
  stats <- get_stats(type="rawbymonth")
  testthat::expect_is(stats, "data.frame")
  testthat::expect_true(nrow(stats) == 1)
  testthat::expect_true(all(c("datasets", "sites", "publications",
                              "authors", "countrygpid", "observations") %in% colnames(stats)))
})

test_that("get_stats dstypemonth", {
  skip_on_cran()
  stats <- get_stats(type="dstypemonth")
  testthat::expect_is(stats, "data.frame")
  testthat::expect_true(all(c("datasettype", "counts") %in% colnames(stats)))
})