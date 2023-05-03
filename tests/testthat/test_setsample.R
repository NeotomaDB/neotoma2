# load libraries
library("testthat")
library("neotoma2")

context("Creating samples from scratch works as expected.")

test_that("Creating samples for the neotoma2 package.", {
  skip_on_cran()
  testthat::expect_error(set_sample(x = 12, depth = 33))
  
  # Update an existing site:
  downloadSample <- get_sites(limit = 5) %>% get_downloads()
  
  testthat::expect_error(set_sample(downloadSample, depth=40))
  oneSample <- downloadSample[[1]]@collunits[[1]]@datasets[[1]]@samples[[1]]
  
  updatedSample <- set_sample(x = oneSample, depth = 40)
  testthat::expect_equal(updatedSample@depth, 40)
  
  testthat::expect_error(set_sample(x = oneSample, depth = "33a"))
  
})
