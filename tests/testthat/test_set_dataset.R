library("testthat")
library("neotoma2")

context("Creating datasets from scratch or updating existing objects.")
test_that("`set_dataset()` for a new dataset", {
  skip_on_cran()
  newDS <- set_dataset(datasetid = 56,
                       datasetname = "My Dataset",
                       database = "neotoma",
                       recdatecreated = as.Date("2025-11-10"),
                       datasettype = "pollen")
  testthat::expect_is(newDS, "dataset")
  testthat::expect_equal(newDS@datasetname, "My Dataset")
  testthat::expect_equal(newDS@datasettype, "pollen")
  testthat::expect_equal(newDS@datasetid, 56)
  testthat::expect_equal(newDS@database, "neotoma")
  testthat::expect_equal(newDS@recdatecreated, as.Date("2025-11-10"))
  testthat::expect_error(set_dataset(x = 12, waterdepth = 33))
})

test_that("Updating existing dataset", {
  skip_on_cran()
  # Cannot update over a sites object:
  downloadSite <- get_sites(limit = 1)
  testthat::expect_error(set_dataset(x = downloadSite, datasetid = -9999))
  # Update single site
  updatedSample <- set_dataset(x = downloadSite[[1]]@collunits[[1]]@datasets[[1]], datasetid = -9999)
  testthat::expect_equal(updatedSample@datasetid, -9999)
  # Cannot use invalid data types
  testthat::expect_error(set_dataset(x = downloadSite[[1]]@collunits[[1]]@datasets[[1]], waterdepth = "33a"))
})