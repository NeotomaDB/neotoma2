library("testthat")
library("neotoma2")

context("Creating samples from scratch or updating existing sample objects.")
test_that("Creating samples for the neotoma2 package.", {
  skip_on_cran()
  testthat::expect_error(set_sample(x = 12, depth = 33))
  sm <- set_sample(depth = 140,
                   sampleid = 42,
                   thickness = 32,
                   samplename = "My Sample")
  testthat::expect_is(sm, "sample")
  testthat::expect_equal(sm@depth, 140)
  testthat::expect_equal(sm@sampleid, 42)
  testthat::expect_equal(sm@thickness, 32)
})

test_that("Updating samples for the neotoma2 package.", {
  skip_on_cran()
  downloadSample <- get_sites(24) %>% get_downloads()
  # Cannot update on sites object
  testthat::expect_error(set_sample(downloadSample, depth=40))
  # Update single sample
  oneSample <- downloadSample[[1]]@collunits[[1]]@datasets[[1]]@samples[[1]]
  testthat::expect_true(is.na(oneSample@depth))
  updatedSample <- set_sample(x = oneSample, depth = 40)
  testthat::expect_equal(updatedSample@depth, 40)
  # Cannot use invalid data types
  testthat::expect_error(set_sample(x = oneSample, depth = "33a"))
})