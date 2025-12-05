library("testthat")
library("neotoma2")

context("Concatenation of c()  in `neotoma2` objects works appropriately.")
test_that("c() concats properly.", {
  skip_on_cran()
  alex <- get_sites(24)
  site5 <- get_sites(5)
  sites <- c(alex, site5)
  testthat::expect_equal(length(sites), 2)
})

test_that("c() removes duplicates properly.", {
  skip_on_cran()
  alex <- get_sites(24)
  alex2 <- get_datasets(7870)
  sites <- c(alex, alex2)
  testthat::expect_equal(length(sites), 1)
  
  site5 <- get_sites(5)
  sites <- c(sites, site5)
  testthat::expect_equal(length(sites), 2)
})