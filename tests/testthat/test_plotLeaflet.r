# load libraries
testthat::test_that("We can download records and plot them with plot leaflet.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  fiftyds <- get_datasets(limit = 50)

  output <- plotLeaflet(fiftyds)
  testthat::expect_is(output, "leaflet")

})

testthat::test_that("We can download records and plot a single site with plot leaflet.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  fiftyds <- get_datasets(limit = 50)

  output <- plotLeaflet(fiftyds[[1]])
  testthat::expect_is(output, "leaflet")
})

testthat::test_that("Location parsing isn't affecting the representation of spatial polygons passed to the DB:", {
  # This is an issue raised by Adrian.

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
  testthat::expect_equivalent(loc1,
    geojsonsf::geojson_sf(parse_location(location)))
})

testthat::test_that("We are pulling in the sites we expect to capture:", {
  skip_on_cran()
  location <- '{"type": "Polygon",
            "coordinates": [[
                [-169, 24],
                [-169, 75],
                [-52, 75],
                [-52, 24],
                [-169, 24]]]}'

  usa <- get_sites(loc = location, limit = 20000)
  usa_ds <- get_datasets(loc = location, all_data = TRUE)
  fla <- get_sites(gpid = "Florida", limit = 10000)

  testthat::expect_true(all(getids(fla)$siteid %in% getids(usa)$siteid))
  testthat::expect_true(all(getids(fla)$siteid %in% getids(usa_ds)$siteid))

})