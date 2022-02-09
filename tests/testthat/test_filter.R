# load libraries
library("testthat")
library("neotoma2")

context("Run Neotoma `test_sites` only when not on CRAN")


test_that("filter runs as expected. filters datasettype", {

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
  
  brazil_datasets <- get_datasets(loc = brazil[1])
  
  brazil_pollen <- filter(brazil_datasets, datasettype == "pollen")
    
  brazil_summary <- summary(brazil_pollen)
  
  brazil_datatype <- unique(brazil_summary$datasets_type)
  
  expect_equal(brazil_datatype, "pollen")
  
}
)


test_that("filter works as expected. filter by lat.", {
  
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
  
  brazil_datasets <- get_datasets(loc = brazil[1])
  
  brazil_lat <- filter(brazil_datasets, lat > 0 & lat < 50)
  
  ds.0.50 <- as.data.frame(brazil_lat)
  latitudes <- ds.0.50$lat
  
  for (i in latitudes) {
    expect_lt(i, 50)
    expect_gt(i, 0)
  }
  
}
)

test_that("filter works as expected. filter by long.", {
  
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
  
  brazil_datasets <- get_datasets(loc = brazil[1])
  
  brazil_long <- filter(brazil_datasets, long > 0 & long < 50)
  
  ds.0.50 <- as.data.frame(brazil_long)
  longitudes <- ds.0.50$long
  
  for (i in longitudes) {
    expect_lt(i, 50)
    expect_gt(i, 0)
  }
  
}
)

test_that("filter works as expected. filter by multiple arguments", {
  
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
  
  brazil_datasets <- get_datasets(loc = brazil[1])
  
  brazil <- filter(brazil_datasets, datasettype == "pollen", long > -60 & long < -50, lat >-10 & lat < 0)
  
  df <- as.data.frame(brazil)
  longitudes <- df$long
  latitudes <- df$lat
  
  
  for (i in longitudes) {
    expect_lt(i, -50)
    expect_gt(i, -60)
  }
  
  for (i in latitudes) {
    expect_lt(i, 0)
    expect_gt(i, -10)
  }
  
  brazil_summary <- summary(brazil)
  brazil_datatype <- unique(brazil_summary$datasets_type)
  expect_equal(brazil_datatype, "pollen")
  
}
)

test_that("filter runs as expected. filters 2 datasettypes", {
  
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
  
  brazil_datasets <- get_datasets(loc = brazil[1])
  
  brazil_ <- filter(brazil_datasets, datasettype == "pollen" | datasettype == "charcoal")
  
  brazil_ids <- getids(brazil_)
  ids <- brazil_ids$datasetid
  
  datasettypes <-  get_datasets(ids)
  brazil_datatype <- summary(datasettypes)
  brazil_datatype <- unique(brazil_datatype$datasets_type)
  
  for (i in brazil_datatype) {
    if (i == "pollen") {
      expect_equal(i, "pollen")
    } else {
      expect_equal(i, "charcoal")
    }
  }

}
)