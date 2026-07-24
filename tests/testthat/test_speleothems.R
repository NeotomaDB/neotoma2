testthat::skip("Skipping all tests in this file")
library("testthat")
library("neotoma2")

context("Run Neotoma `test_speleothems` only when not on CRAN")
test_that("speleothems method extracts data correctly", {
  skip_on_cran()
  spel_data<- get_datasets(datasettype='speleothem', limit=2)
  spel_data <- get_speleothems(spel_data)
  if (!is.null(spel_data) && length(spel_data) > 0) {
    spel_extracted <- speleothems(spel_data)
    testthat::expect_is(spel_extracted, "data.frame")
    testthat::expect_equal(nrow(spel_extracted), 2)
  }
})

test_that("no duplicate rows in speleothems data frame", {
  skip_on_cran()
  spel_data<- get_datasets(datasettype='speleothem', limit=10)
  spel_data <- speleothems(get_speleothems(spel_data))
  testthat::expect_equal(nrow(spel_data), nrow(unique(spel_data)))
})

# Tests for speleothemdetails() function
test_that("speleothemdetails method works on sites object", {
  skip_on_cran()
  spel_data<- get_datasets(datasettype='speleothem', limit=10)
  spel_sites <- get_speleothems(spel_data)
  if (!is.null(spel_sites) && length(spel_sites) > 0) {
    testthat::expect_warning(
      result <- speleothemdetails(spel_sites)
    )
    testthat::expect_is(result, "data.frame")
    testthat::expect_true(nrow(result) >= 0)
  }
})

test_that("speleothemdetails returns distinct rows for sites", {
  skip_on_cran()
  spel_sites <- get_speleothems(c(66775, 66789))
  if (!is.null(spel_sites) && length(spel_sites) > 0) {
    testthat::expect_warning(
      result <- speleothemdetails(spel_sites)
    )
    testthat::expect_equal(nrow(result), nrow(unique(result)))
  }
})

test_that("speleothemdetails warns when no speleothems found in sites", {
  skip_on_cran()
  non_spel_sites <- get_sites(siteid = 1)
  if (!is.null(non_spel_sites) && length(non_spel_sites) > 0) {
    testthat::expect_warning(
      speleothemdetails(non_spel_sites),
      regexp = "No assigned speleothems"
    )
  }
})

test_that("speleothemdetails method works on single site object", {
  skip_on_cran()
  spel_data <- get_datasets(66789) %>%
    get_speleothems()
  if (!is.null(spel_data) && length(spel_data) > 0) {
    result <- speleothemdetails(spel_data[[1]])
    testthat::expect_is(result, "data.frame")
  }
})

test_that("speleothemdetails includes siteid in site method", {
  skip_on_cran()
  spel_data <- get_datasets(66789) %>%
    get_speleothems()
  if (!is.null(spel_data) && length(spel_data) > 0) {
    result <- speleothemdetails(spel_data[[1]])
    if (nrow(result) > 0) {
      testthat::expect_true("siteid" %in% names(result))
      testthat::expect_true(all(!is.na(result$siteid)))
    }
  }
})

test_that("speleothemdetails method works on collunits object", {
  skip_on_cran()
  spel_data <- get_datasets(66789) %>%
    get_speleothems()
  if (!is.null(spel_data) && length(spel_data) > 0) {
    result <- speleothemdetails(spel_data[[1]]@collunits[[1]])
    if (nrow(result) > 0) {
      testthat::expect_is(result, "data.frame")
      testthat::expect_true(length(unique(result$collunitid.x)) == 1)
      testthat::expect_true(length(unique(result$datasetid)) == 1)
    }
  }
})  

test_that("speleothemdetails joins speleothems and samples for collunit", {
  skip_on_cran()
  spel_site <- get_datasets(datasettype="speleothem", limit=1) %>%
    get_speleothems()
  if (!is.null(spel_site) && length(spel_site) > 0 && 
      length(spel_site[[1]]@collunits@collunits) > 0) {
    collunit_obj <- spel_site[[1]]@collunits@collunits[[1]]
    testthat::expect_warning(
      result <- speleothemdetails(collunit_obj)
    )
    if (nrow(result) > 0) {
      testthat::expect_true("datasetid" %in% names(result))
      # Sample-related columns
      sample_cols <- c("age", "depth", "sampleid")
      has_sample_cols <- all(sample_cols %in% names(result))
      testthat::expect_true(has_sample_cols)
    }
  }
})