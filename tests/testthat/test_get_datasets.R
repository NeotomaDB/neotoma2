library("testthat")
library("neotoma2")

context("get_datasets() retrieves data from /dataset endpoint 
        and parses content as expected.")
test_that("get_datasets numeric", {
  skip_on_cran()
  dataset_1001 <- neotoma2::get_datasets(1001)
  datasets_ids <- neotoma2::getids(dataset_1001) %>%
    dplyr::select(datasetid) %>%
    unique()
  testthat::expect_equivalent(datasets_ids, 1001)
})

test_that("get_datasets numeric vector", {
  skip_on_cran()
  datasets_ob <- get_datasets(c(1001, 2001, 15, 24))
  testthat::expect_length(datasets_ob, 4)
  datasets_vec <- getids(datasets_ob) %>%
    dplyr::select(datasetid) %>%
    unique() %>%
    unlist()
  testthat::expect_setequal(datasets_vec, c(1001, 2001, 15, 24))
  datasets_long <- get_datasets(seq(1, 1000), limit = 10)
  testthat::expect_length(datasets_long, 10)
})

test_that("get_datasets with loc attribute", {
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
  brazil_datasets <- get_datasets(loc = brazil[1], datasettype = "pollen")
  # Check that datasset types names are only pollen
  sum <- summary(brazil_datasets)
  testthat::expect_lte(length(brazil_datasets), nrow(sum))
  testthat::expect_equivalent(nrow(sum), nrow(getids(brazil_datasets)))
  # All datasets should be pollen:
  testthat::expect_equivalent(unique(sum$dataset_types), "pollen")
  # Check that all siteids are unique
  brazil_unique_sites <- length(unique(getids(brazil_datasets)$siteid))
  brazil_sites_length <- length(brazil_datasets)
  testthat::expect_equal(brazil_sites_length, brazil_unique_sites)
  # Check that all datasetids are unique
  brazil_unique_dsids <- length(unique(getids(brazil_datasets)$datasetid))
  brazil_ds_length <- length(datasets(brazil_datasets))
  testthat::expect_equal(brazil_ds_length, brazil_unique_dsids)
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
  data_short <- get_datasets(loc = europe_json[1])
  data_long <- get_datasets(loc = europe_json[1], all_data = TRUE)
  testthat::expect_gt(length(data_long), length(data_short))
  eur_ids <- getids(data_long)
  # check that all datasetids in datasets df are in eur_ids
  ds_ids <- as.data.frame(datasets(data_short)) %>%
    dplyr::select(datasetid) %>%
    unique() %>%
    unlist()
  testthat::expect_true(all(ds_ids %in% eur_ids$datasetid))
  cu_ids <- as.data.frame(collunits(data_short)) %>%
    dplyr::select(collectionunitid) %>%
    unique() %>%
    unlist()
  testthat::expect_true(all(cu_ids %in% eur_ids$collunitid))
})