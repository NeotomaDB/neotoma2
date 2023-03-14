# load libraries
library("testthat")
library("neotoma2")

context("Creating hashes for site objects.")

test_that("Hash remains equal for sites objects.", {
  
  alex_mult <- get_sites(sitename="Alex%")
  alex <- get_sites(24)
  alex_dw <- get_downloads(alex)
  alex_ds <- get_datasets(alex)
  
  testthat::expect_equal(attributes(alex_mult[[1]])$hash, 
                         attributes(alex[[1]])$hash)
  
  testthat::expect_equal(attributes(alex_mult[[1]])$hash, 
                         attributes(alex_dw[[1]])$hash)
  
  testthat::expect_equal(attributes(alex_ds[[1]])$hash, 
                         attributes(alex_dw[[1]])$hash)

})

test_that("Hash remains equal for collunit objects.", {
  
  alex <- get_sites(24)
  alex_mult <- get_sites(sitename="Alex%")
  alex_dw <- get_downloads(alex)
  alex_dw2 <- get_downloads(alex_mult)
  
  testthat::expect_equal(attributes(alex_mult[[1]]@collunits[[1]])$hash, 
                         attributes(alex[[1]]@collunits[[1]])$hash)

})