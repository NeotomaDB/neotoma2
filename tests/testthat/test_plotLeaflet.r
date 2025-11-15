library("testthat")
library("neotoma2")

context("Test leaflet plots")

testthat::test_that("Plot records with plotLeaflet.", {
  skip_on_cran()
  ten <- get_sites(limit = 10)
  output <- plotLeaflet(ten)
  testthat::expect_is(output, "leaflet")
})

testthat::test_that("Plot a single record with plotLeaflet.", {
  skip_on_cran()
  five <- get_sites(limit = 5)
  output <- plotLeaflet(five[[1]])
  testthat::expect_is(output, "leaflet")
})
