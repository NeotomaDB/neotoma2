# load libraries
library("testthat")
library("neotoma2")

context("Run Neotoma `test_samples` only when not on CRAN")

test_that("Running samples on a record with multiple chronologies
  pulls the default model.", {

  ## we don't want this to run on CRAN
  skip_on_cran()
  singlechron <- testthat::expect_true({samples(get_downloads(4716)); TRUE})
  multichron <- testthat::expect_true({samples(get_downloads(21007)); TRUE})
  mammals <- testthat::expect_true({samples(get_downloads(4564)); TRUE})
})
