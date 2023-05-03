test_that("get_datasets numeric runs.", {
  ## we don't want this to run on CRAN
  skip_on_cran()

  dataset_1001 <- neotoma2::get_datasets(1001)

  # Check that sites' id matches 1001
  datasets_ids <- neotoma2::getids(dataset_1001) %>%
    dplyr::select(datasetid) %>%
    unique()

  testthat::expect_equivalent(datasets_ids, "1001")

})

test_that("get_datasets runs as a vector with length
  defined by vector or limit.", {
  ## we don't want this to run on CRAN
  skip_on_cran()

  datasets_ob <- get_datasets(c(1001, 2001, 15, 24))

  datasets_vec <- getids(datasets_ob) %>%
    dplyr::select(datasetid) %>%
    unique() %>%
    unlist()

  datasets_long <- get_datasets(seq(1, 1000), limit = 10)

  # Check that sites' id matches 1001
  expect_setequal(datasets_vec, c(1001, 2001, 15, 24))
  expect_length(datasets_ob, 4)
  expect_length(datasets_long, 10)

})

test_that("get_datasets loc runs.", {
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

  brazil_sf <- geojsonsf::geojson_sf(brazil)

  brazil_datasets <- get_datasets(loc = brazil[1], datasettype = "pollen")

  # Check that datasset types names are only pollen
  sum <- summary(brazil_datasets)

  # Should be at least as many datasets as sites:
  expect_lte(length(brazil_datasets), nrow(sum))
  expect_equivalent(nrow(sum), nrow(getids(brazil_datasets)))
  # All datasets should be pollen:
  expect_equivalent(unique(sum$type), "pollen")

})

# Testing arguments such as `altmin`, `altmax`

test_that("get_datasets runs as expected using altitude.", {
  ## we don't want this to run on CRAN
  skip_on_cran()

  altmin <- 100
  altmax <- 250

  ds <- get_datasets(altmin = altmin, altmax = altmax)
  ds_df <- as.data.frame(ds)
  altitudes <- ds_df$elev

  expect_gte(min(as.data.frame(ds)$elev), 100)
  expect_lte(max(as.data.frame(ds)$elev), 250)


})

test_that("get_datasets runs as expected.", {
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

  brazil_sf <- geojsonsf::geojson_sf(brazil)

  brazil_datasets <- get_datasets(loc = brazil[1], datasettype = "pollen")

  # Check that datasset types names are only pollen

  brazil_unique_sites <- length(unique(getids(brazil_datasets)$siteid))
  brazil_datasets_length <- length(brazil_datasets)
  expect_equal(brazil_datasets_length, brazil_unique_sites)
})

test_that("all_data + loc work", {
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

})
