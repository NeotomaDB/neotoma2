## load packages
library("testthat")
library("neotoma2")

context("Run Neotoma `test_sites` only when not on CRAN")


test_that("get_sites runs as expected.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  dataset.1001 <- get_datasets(1001)
  
  # Check that sites' id matches 1001
  datasets.ids <- getids(dataset.1001)
  datasets.ids <- unique(datasets.ids$datasetid)
  
  expect_equal(datasets.ids, 1001)
  
})

test_that("get_sites runs as expected.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  datasets.ob <- get_datasets(c(1001, 2001, 15, 24))
  datasets.vec <- getids(datasets.ob)
  datasets.vec <- unique(datasets.vec$datasetid)
  
  # Check that sites' id matches 1001
  
  expect_setequal(datasets.vec, c(1001, 2001, 15, 24))
  
})

test_that("get_datasets runs as expected.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  brazil <- '{"type": "Polygon", 
            "coordinates": [[
                [-73.125, -9.102],
                [-56.953, -33.138],
                [-36.563, -7.711],
                [-68.203, 13.923],
                [-73.125, -9.102]
              ]]}'
  
  brazil_sf <- geojsonsf::geojson_sf(brazil)
  
  brazil_datasets <- get_datasets(loc = brazil[1], datasettype = "pollen")
  
  # Check that datasset types names are only pollen
  
  brazil_datatype <- summary(brazil_datasets)
  brazil_datatype <- unique(brazil_datatype$datasets_type)
  expect_equal(brazil_datatype, "pollen")
  
})

# Testing arguments such as `altmin`, `altmax`

test_that("get_datasets runs as expected.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  altmin = 100
  altmax = 250
  
  ds.100.250 <- get_datasets(altmin = altmin, altmax = altmax)
  ds.100.250.df <- as.data.frame(ds.100.250)
  altitudes <- ds.100.250.df$elev
  
  # Check that datasset types names are only pollen
  
  for (i in altitudes) {
    expect_lt(i, 250)
    expect_gt(i, 100)
  }
})
