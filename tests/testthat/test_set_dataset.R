library("testthat")
library("neotoma2")

context("Creating datasets from scratch or updating existing objects.")
test_that("`set_dataset()` for a new dataset", {
  skip_on_cran()
  new_ds <- set_dataset(
    datasetid = 56,
    datasetname = "My Dataset",
    database = "neotoma",
    recdatecreated = as.Date("2025-11-10"),
    datasettype = "pollen"
  )
  testthat::expect_is(new_ds, "dataset")
  testthat::expect_equal(new_ds@datasetname, "My Dataset")
  testthat::expect_equal(new_ds@datasettype, "pollen")
  testthat::expect_equal(new_ds@datasetid, 56)
  testthat::expect_equal(new_ds@database, "neotoma")
  testthat::expect_equal(new_ds@recdatecreated, as.Date("2025-11-10"))
  testthat::expect_error(set_dataset(x = 12, waterdepth = 33))
})

test_that("Updating existing dataset", {
  skip_on_cran()
  # Cannot update over a sites object:
  download_site <- get_sites(limit = 1)
  testthat::expect_error(set_dataset(x = download_site, datasetid = -9999))
  # Update single site
  ds <- download_site[[1]]@collunits[[1]]@datasets[[1]]
  updated_sample <- set_dataset(x = ds, datasetid = -9999)
  testthat::expect_equal(updated_sample@datasetid, -9999)
  # Cannot use invalid data types
  testthat::expect_error(set_dataset(x = ds, waterdepth = "33a"))
})
