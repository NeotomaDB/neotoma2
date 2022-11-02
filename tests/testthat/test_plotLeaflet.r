# load libraries
library("testthat")
library("neotoma2")

context("Run Neotoma plotLeaflet tests when not on Cran")

test_that("We can download records and plot them with plot leaflet.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  fiftyds <- get_datasets(limit = 50)

  output <- plotLeaflet(fiftyds)
  testthat::expect_is(output, "leaflet")

})

test_that("We can download records and plot a single site with plot leaflet.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  fiftyds <- get_datasets(limit = 50)

  output <- plotLeaflet(fiftyds[[1]])
  testthat::expect_is(output, "leaflet")
})

test_that("The curvature of the earth isn't affecting the projection/selection", {
  # This is an issue raised by Adrian.
  skip_on_cran()
  location <- '{"type": "Polygon",
            "coordinates": [[
                [-169, 24],
                [-169, 75],
                [-52, 75],
                [-52, 24],
                [-169, 24]]]}'

  loc1 <- geojsonsf::geojson_sf(location) # sf object
  loc <- geojsonsf::sf_geojson(loc1)

  testthat::expect_equivalent(loc1, geojsonsf::geojson_sf(loc))

  usa <- get_sites(loc = location, limit = 10000)
  usa_ds <- get_datasets(loc = location, limit = 10000)
  fla <- get_sites(gpid = "Florida", limit = 10000)

  testthat::expect_true(all(getids(fla)$siteid %in% getids(usa)$siteid))
  testthat::expect_true(all(getids(fla)$siteid %in% getids(usa_ds)$siteid))

})