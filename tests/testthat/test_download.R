## load packages
library("testthat")
library("neotoma2")

context("Run Neotoma download examples only when not on CRAN")

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

test_that("get_downloads yields same get_datasets ids I", {
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
  
  df1 <- get_datasets(loc = brazil[1]) %>% get_downloads() %>% getids()
  df2 <- get_datasets(loc = brazil[1]) %>% getids()

  expect_equal(df1, df2)
  
})

test_that("get_downloads yields same get_datasets ids II", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  core_sites <- c(13949,11904,13319,728,13248,2625,2806,13280,519,11745,
                  273,13956,11880,13321,9801,13698,11816,13909,13921) 
  
  df1 <- get_sites(core_sites) %>%
    get_datasets() %>%
    get_downloads() %>%
    getids()
  
  df2 <- get_sites(core_sites) %>%
    get_datasets() %>%
    getids()
  
  expect_equal(df1, df2)
  
})