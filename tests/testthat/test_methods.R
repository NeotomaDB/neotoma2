test_that("Working with subset methods `datasets()` and `collunits()`.", {
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
  brazil_sf <- (brazil)
  brazil_dl <- brazil_sf %>%
    get_sites(loc = .) %>%
    get_downloads()

  sumDl <- summary(brazil_dl)
  brazilids <- getids(brazil_dl)
  brazil_cu <- collunits(brazil_dl)
  brazil_ds <- datasets(brazil_dl)

  expect_identical(object = length(brazil_ds),
                   expected = nrow(brazilids),
                   label = "The datasets() function isn't returning the right
                            dataset list")

  expect_identical(object = length(unique(sumDl$siteid)),
                   expected = length(brazil_dl),
                   label = "Failed to match the site identifiers & 
                     site length.")

  expect_identical(object = sum(sumDl$datasets),
                   expected = nrow(brazilids),
                   label = "Datasets returned are not the same length.")
})
