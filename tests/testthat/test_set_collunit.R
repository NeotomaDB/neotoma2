library("testthat")
library("neotoma2")

context("Creating collection units from scratch or updating existing objects.")
test_that("`set_collunit()` for new collection unit", {
  skip_on_cran()
  new_cu <- set_collunit(
    notes = "CU set up",
    handle = "My CU",
    colldate = as.Date("2025-11-14"),
    gpslocation = sf::st_as_sf(
      sf::st_sfc(
        sf::st_point(c(-100.0, 40.0)),
        crs = 4326
      )
    ),
    collunittype = "magical"
  )
  datasets <- new("datasets", datasets = list(new("dataset")))
  testthat::expect_is(new_cu, "collunit")
  testthat::expect_equal(new_cu@handle, "My CU")
  testthat::expect_equal(new_cu@collunittype, "magical")
  testthat::expect_equal(sf::st_coordinates(new_cu@gpslocation)[1], -100.0)
  testthat::expect_equal(sf::st_coordinates(new_cu@gpslocation)[2], 40.0)
})

test_that("Updating existing collection unit", {
  skip_on_cran()
  # Invalid input:
  testthat::expect_error(set_collunit(x = 12, waterdepth = 33))
  # Cannot update over a sites object:
  download_site <- get_sites(limit = 1)
  testthat::expect_error(set_collunit(x = download_site, waterdepth = 33))
  # Update single site
  cu <- download_site[[1]]@collunits[[1]]
  updated_sample <- set_collunit(x = cu, waterdepth = -9999)
  testthat::expect_equal(updated_sample@waterdepth, -9999)
  # Cannot use invalid data types
  testthat::expect_error(set_collunit(x = cu, waterdepth = "33a"))
})
