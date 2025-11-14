library("testthat")
library("neotoma2")

context("Creating collection units from scratch or updating existing objects.")
test_that("`set_collunit()` for new collection unit", {
  skip_on_cran()
  newCU <- set_collunit(notes = "CU set up",
                          handle = "My CU",
                          colldate = as.Date("2025-11-14"),
                          gpslocation = sf::st_as_sf(
                            sf::st_sfc(
                              sf::st_point(c(-100.0, 40.0)),
                              crs = 4326)),
                          collunittype = "magical")
                          datasets = new("datasets", datasets = list(new("dataset")))
  testthat::expect_is(newCU, "collunit")
  testthat::expect_equal(newCU@handle, "My CU")
  testthat::expect_equal(newCU@collunittype, "magical")
  testthat::expect_equal(sf::st_coordinates(newCU@gpslocation)[1], -100.0)
  testthat::expect_equal(sf::st_coordinates(newCU@gpslocation)[2], 40.0)
})

test_that("Updating existing collection unit", {
  skip_on_cran()
  # Invalid input:
  testthat::expect_error(set_collunit(x = 12, waterdepth = 33))
  # Cannot update over a sites object:
  downloadSite <- get_sites(limit = 1)
  testthat::expect_error(set_collunit(x = downloadSite, waterdepth = 33))
  # Update single site
  updatedSample <- set_collunit(x = downloadSite[[1]]@collunits[[1]], waterdepth = -9999)
  testthat::expect_equal(updatedSample@waterdepth, -9999)
  # Cannot use invalid data types
  testthat::expect_error(set_collunit(x = downloadSite[[1]]@collunits[[1]], waterdepth = "33a"))
})