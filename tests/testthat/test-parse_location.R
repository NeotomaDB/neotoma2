test_that("Parse location fails with an incomplete bounding box:", {
  expect_error(parse_location(c(1, 2)))
})

test_that("We can pass in an sf object and return the WKT:", {
  g <- sf::st_sfc(st_point(1:2))
  sf_obj <- sf::st_sf(a = 3, g)
  expect_is(parse_location(sf_obj), "geojson")
})

test_that("Passing in a WKT works:", {
  poly <- parse_location("POLYGON ((40 40, 20 45, 45 30, 40 40))")
  expect_is(parse_location(poly), "geojson")
})
