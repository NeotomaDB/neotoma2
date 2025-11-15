library("testthat")
library("neotoma2")

context("Test plots")
testthat::test_that("Plot records with plot", {
  skip_on_cran()
  ten <- get_sites(limit = 10)
  testthat::expect_error(plot(ten), NA)
})

testthat::test_that("Plot a single record with plot.", {
  skip_on_cran()
  five <- get_sites(limit = 5)
  testthat::expect_error(plot(five[[1]]), NA)
})