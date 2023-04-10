# load libraries
#Sys.setenv(NOT_CRAN='skip')
library("testthat")
library("neotoma2")

context("Verifying that concatenation of c() works appropriately.")

test_that("c() concats properly.", {

  skip_on_cran()
  alex <- get_sites(24)
  site5 <- get_sites(5)
  alex2 <- get_datasets(7870)

  sites <- c(alex, alex2)
  testthat::expect_equal(length(sites), 1)

  sites <- c(sites, site5)
  testthat::expect_equal(length(sites), 2)
})