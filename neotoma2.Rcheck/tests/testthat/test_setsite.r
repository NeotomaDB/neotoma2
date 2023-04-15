# load libraries
library("testthat")
library("neotoma2")

context("Creating objects from scratch works as expected.")

test_that("Creating sites for the neotoma2 package.", {
  skip_on_cran()
  testthat::expect_error(set_site(x = 12, altitude = 33))
  
  # Update an existing site:
  downloadSite <- get_sites(limit = 10)
  
  testthat::expect_error(set_site(x = downloadSite, altitude = 33))
  
  updatedSample <- set_site(x = downloadSite[[1]], altitude = -9999)
  testthat::expect_equal(updatedSample@altitude, -9999)
  
  testthat::expect_error(set_site(x = downloadSite[[2]], altitude = "33a"))
  
})
