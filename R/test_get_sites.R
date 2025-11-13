library("testthat")
library("neotoma2")

context("Test `get_sites()` function.")
test_that("get_sites numeric", {
  skip_on_cran()
  site_1001 <- neotoma2::get_sites(1001)
  sites_ids <- neotoma2::getids(site_1001) %>%
    dplyr::select(siteid) %>%
    unique()
  testthat::expect_equivalent(sites_ids, 1001)
})

test_that("get_sites numeric vector", {
  skip_on_cran()
  sites_ob <- get_sites(c(1001, 2001, 15, 24))
  testthat::expect_length(sites_ob, 4)
  sites_vec <- getids(sites_ob) %>%
    dplyr::select(siteid) %>%
    unique() %>%
    unlist()
  testthat::expect_setequal(sites_vec, c(1001, 2001, 15, 24))
  sites_long <- get_sites(seq(1, 1000), limit = 10)
  testthat::expect_length(sites_long, 10)
})

test_that("get_sites with loc attribute", {
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
  brazil_sites <- get_sites(loc = brazil[1], datasettype = "pollen")
  sum <- summary(brazil_sites)
  testthat::expect_lte(length(brazil_sites), nrow(sum))
  testthat::expect_equivalent(nrow(sum), nrow(getids(brazil_sites)))
  # All datasets should be pollen:
  testthat::expect_equivalent(unique(sum$dataset_types), "pollen")
  # Check that all siteids are unique
  brazil_unique_sites <- length(unique(getids(brazil_sites)$siteid))
  brazil_sites_length <- length(brazil_sites)
  testthat::expect_equal(brazil_sites_length, brazil_unique_sites)
})

test_that("all_data + loc", {
  skip_on_cran()
  europe_json <- '{"type": "Polygon",
            "coordinates": [[
                [-73.125, -9.102],
                [-56.953, -33.138],
                [-36.563, -7.711],
                [-68.203, 13.923],
                [-73.125, -9.102]
              ]]}'
  data_short <- get_sites(loc = europe_json[1])
  data_long <- get_sites(loc = europe_json[1], all_data = TRUE)
  testthat::expect_gt(length(data_long), length(data_short))
  eur_ids <- getids(data_long)
  # check that all siteids are in eur_ids
  st_ids <- as.data.frame(data_short) %>%
    dplyr::select(siteid) %>%
    unique() %>%
    unlist()
  testthat::expect_true(all(st_ids %in% eur_ids$siteid))
})

context("get_sites()")
test_that("If B is contained in A region,
          get_sites() from B will be contained
          in get_sites() from A", {
            skip_on_cran()
            location <- '{"type": "Polygon",
            "coordinates": [[
                [-169, 24],
                [-169, 75],
                [-52, 75],
                [-52, 24],
                [-169, 24]]]}'
            usa <- get_sites(loc = location, limit = 20000)
            fla <- get_sites(gpid = "Florida", limit = 10000)
            testthat::expect_true(all(getids(fla)$siteid %in% getids(usa)$siteid))
          })