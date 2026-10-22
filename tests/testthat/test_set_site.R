library("testthat")
library("neotoma2")

context("Creating sites from scratch or updating existing site objects.")
test_that("`set_sites()` for new site.", {
  skip_on_cran()
  new_site <- set_site(
    sitename = "My New Site",
    geography = sf::st_as_sf(
      sf::st_sfc(
        sf::st_point(c(-100.0, 40.0)),
        crs = 4326
      )
    ),
    altitude = 250
  )
  testthat::expect_is(new_site, "site")
  testthat::expect_equal(new_site@sitename, "My New Site")
  testthat::expect_equal(new_site@altitude, 250)
  testthat::expect_equal(sf::st_coordinates(new_site@geography)[1], -100.0)
  testthat::expect_equal(sf::st_coordinates(new_site@geography)[2], 40.0)
})

test_that("Updating existing site.", {
  skip_on_cran()
  # Invalid input:
  testthat::expect_error(set_site(x = 12, altitude = 33))
  # Cannot update over a sites object:
  download_site <- get_sites(limit = 1)
  testthat::expect_error(set_site(x = download_site, altitude = 33))
  # Update single site
  updated_sample <- set_site(x = download_site[[1]], altitude = -9999)
  testthat::expect_equal(updated_sample@altitude, -9999)
  # Cannot use invalid data types
  testthat::expect_error(set_site(x = download_site[[2]], altitude = "33a"))
})
