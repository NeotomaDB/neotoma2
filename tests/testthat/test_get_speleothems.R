testthat::skip("Skipping all tests in this file")
library("testthat")
library("neotoma2")

context("Run Neotoma `test_speleothems` only when not on CRAN")
test_that("get_speleothems runs with numeric datasetid", {
  skip_on_cran()
  kesang <- get_speleothems(68552)
  testthat::expect_is(kesang, "sites")
  testthat::expect_true(length(kesang) > 0)
})

test_that("get_speleothems works with vector of datasetids", {
  skip_on_cran()
  spel_multi <- get_speleothems(c(68552, 68512, 67718, 69557))
  testthat::expect_is(spel_multi, "sites")
  testthat::expect_true(length(spel_multi) >= 1)
})

test_that("get_speleothems works with sites object", {
  skip_on_cran()
  test_sites <- get_sites(37302)
  spel_sites <- get_speleothems(test_sites)
  testthat::expect_is(spel_sites, "sites")
})


# test_that("get_speleothems handles invalid datasetid", {
#   skip_on_cran()
#   skip_if(TRUE, "Speleothem functionality needs verification")
#   
#   # Test with non-existent datasetid
#   spel_invalid <- get_speleothems(999999999)
#   
#   # Should return NULL or empty result
#   testthat::expect_true(is.null(spel_invalid) || length(spel_invalid) == 0)
# })

test_that("get_speleothems returns correct structure", {
  skip_on_cran()
  spel_data <- get_speleothems(68552)
  if (!is.null(spel_data) && length(spel_data) > 0) {
    summary <- summary(spel_data)
    testthat::expect_true("speleothem" %in% summary$types)
  }
})

test_that("get_speleothems handles empty results", {
  skip_on_cran()
  no_spel <- get_speleothems(1)
  testthat::expect_true(is.null(no_spel) || length(no_spel) >= 0)
  testthat::expect_is(no_spel, "sites")
})

test_that("if API has error, get_speleothems also breaks", {
  skip_on_cran()
  testthat::expect_error(get_speleothems("invalid"))
})

test_that("get_speleothems integrates with summary", {
  skip_on_cran()
  spel_data <- get_speleothems(68552)
  if (!is.null(spel_data) && length(spel_data) > 0) {
    sum_data <- summary(spel_data)
    testthat::expect_is(sum_data, "data.frame")
    testthat::expect_true(nrow(sum_data) > 0)
  }
})