library("testthat")
library("neotoma2")

context("Test `get_sites()` function.")

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
  skip_if_api_unreachable()
  # `brazil_json` / `brazil_sf` come from setup.R.
  brazil_sites <- get_sites(loc = brazil_json[1], datasettype = "pollen")
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
  # Heavy all_data pagination over a spatial query: flap-prone, skip on CI.
  skip_on_ci()
  # Validates `all_data` pagination on the sites endpoint (superset check), so
  # it keeps the loop; space it out. Uses the shared `brazil_json` from setup.R
  on.exit(Sys.sleep(10), add = TRUE)
  data_short <- get_sites(loc = brazil_json[1])
  data_long <- get_sites(loc = brazil_json[1], all_data = TRUE)
  testthat::expect_gt(length(data_long), length(data_short))
  br_ids <- getids(data_long)
  # check that all siteids are in br_ids
  st_ids <- as.data.frame(data_short) %>%
    dplyr::select(siteid) %>%
    unique() %>%
    unlist()
  testthat::expect_true(all(st_ids %in% br_ids$siteid))
})

# context("get_sites()")
# test_that("If B is contained in A region,
#           get_sites() from B will be contained
#           in get_sites() from A", {
#             skip_on_cran()
#             # Heavy spatial query (limit = 20000); space it out to reduce load
#             # spikes on the API.
#             on.exit(Sys.sleep(10), add = TRUE)
#             location <- '{"type": "Polygon",
#             "coordinates": [[
#                 [-169, 24],
#                 [-169, 75],
#                 [-52, 75],
#                 [-52, 24],
#                 [-169, 24]]]}'
#             usa <- get_sites(loc = location, limit = 20000)
#             fla <- get_sites(gpid = "Florida", limit = 10000)
#             testthat::expect_true(all(getids(fla)$siteid %in% getids(usa)$siteid))
#           })