## load packages
library("testthat")
library("neotoma2")

context("Run Neotoma examples only when not on CRAN")

test_that("get_downloads runs as expected.", {
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
  
  # We can make the geojson a spatial object if we want to use the
  # functionality of the `sf` package.
  brazil_sf <- geojsonsf::geojson_sf(brazil)
  
  brazil_datasets <- get_datasets(loc = brazil[1])
  
  brazil_dl <- get_downloads(brazil_datasets)
  expect_identical(nrow(getids(brazil_datasets)), nrow(getids(brazil_dl)))
  
})