testthat::test_that("Parse location fails with an incomplete bounding box:", {
  testthat::expect_error(parseLocation(c(1, 2)))
})

# testthat::test_that("We can pass in an sf object and return the WKT:", {
#   g <- sf::st_sfc(st_point(1:2))
#   sf_obj <- sf::st_sf(a = 3, g)
#   testthat::expect_is(parseLocation(sf_obj), "geojson")
# })
# 
# testthat::test_that("Passing in a WKT works:", {
#   poly <- parse_location("POLYGON ((40 40, 20 45, 45 30, 40 40))")
#   testthat::expect_is(parseLocation(poly), "geojson")
# })
