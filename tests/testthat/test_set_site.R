library("testthat")
library("neotoma2")

context("Creating sites from scratch or updating existing site objects.")
test_that("`set_sites()` for new site.", {
  skip_on_cran()
  newSite <- set_site(sitename = "My New Site",
                      geography = sf::st_as_sf(
                        sf::st_sfc(
                          sf::st_point(c(-100.0, 40.0)),
                          crs = 4326)),
                      altitude = 250)
  testthat::expect_is(newSite, "site")
  testthat::expect_equal(newSite@sitename, "My New Site")
  testthat::expect_equal(newSite@altitude, 250)
  testthat::expect_equal(sf::st_coordinates(newSite@geography)[1], -100.0)
  testthat::expect_equal(sf::st_coordinates(newSite@geography)[2], 40.0)
})

test_that("Updating existing site.", {
  skip_on_cran()
  # Invalid input:
  testthat::expect_error(set_site(x = 12, altitude = 33))
  # Cannot update over a sites object:
  downloadSite <- get_sites(limit = 1)
  testthat::expect_error(set_site(x = downloadSite, altitude = 33))
  # Update single site
  updatedSample <- set_site(x = downloadSite[[1]], altitude = -9999)
  testthat::expect_equal(updatedSample@altitude, -9999)
  # Cannot use invalid data types
  testthat::expect_error(set_site(x = downloadSite[[2]], altitude = "33a"))
})