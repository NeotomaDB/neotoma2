# load libraries
library("testthat")
library("neotoma2")

context("Run only when not on CRAN")

test_that("Some datasets don't seem to get pulled with chronologies.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  getchron <- get_downloads(4716) %>%
    chronologies() %>%
    as.data.frame()

  singlechron <- testthat::expect_true(any(getchron$chronologyid == 2195))

})
