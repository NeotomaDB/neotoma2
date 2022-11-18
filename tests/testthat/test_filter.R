test_that("filter runs as expected. filters datasettype", {

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
  brazil_datasets <- get_datasets(loc = brazil[1])
  brazil_pollen <- neotoma2::filter(brazil_datasets, datasettype == "pollen")
  brazil_summary <- summary(brazil_pollen)
  brazil_datatype <- unique(brazil_summary$types)

  expect_equal(brazil_datatype, "pollen")

})

test_that("filter works as expected. filter by lat.", {

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
  brazil_datasets <- get_datasets(loc = brazil[1])
  brazil_lat <- neotoma2::filter(brazil_datasets, lat > 0 & lat < 50)
  ds.0.50 <- as.data.frame(brazil_lat)
  latitudes <- ds.0.50$lat

  for (i in latitudes) {
    expect_lt(i, 50)
    expect_gt(i, 0)
  }

})

test_that("filter works as expected. filter by long.", {

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

  brazil_datasets <- get_datasets(loc = brazil[1])

  brazil_long <- neotoma2::filter(brazil_datasets, long > 0 & long < 50)

  expect_length(brazil_long, 0)

})

test_that("filter works as expected. filter by multiple arguments", {

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

  brazil_datasets <- get_datasets(loc = brazil[1])

  brazil <- neotoma2::filter(brazil_datasets,
                             datasettype == "pollen" &
                             long > -60 & long < -40 &
                             lat > -20 & lat < 0)

  df <- as.data.frame(brazil)
  longitudes <- df$long
  latitudes <- df$lat

  for (i in longitudes) {
    expect_lt(i, -40)
    expect_gt(i, -60)
  }

  for (i in latitudes) {
    expect_lt(i, 0)
    expect_gt(i, -20)
  }

  brazil_summary <- summary(brazil)
  brazil_datatype <- unique(brazil_summary$types)
  expect_equal(brazil_datatype, "pollen")

})

test_that("filter runs as expected. filters 2 datasettypes", {

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

  brazil_datasets <- get_datasets(loc = brazil[1])

  brazil_ <- neotoma2::filter(brazil_datasets,
                              datasettype == "pollen" | datasettype == "charcoal")

  brazil_datatype <- summary(brazil_)
  brazil_datatype <- unique(brazil_datatype$datasets_type)

  for (i in seq_len(length(brazil_))) {
    for (j in seq_len(length(brazil_[[i]]@collunits))) {
      for (k in seq_len(length(brazil_[[i]]@collunits[[j]]@datasets))) {
        if (brazil_[[i]]@collunits[[j]]@datasets[[k]]@datasettype == "pollen") {
          expect_equal(brazil_[[i]]@collunits[[j]]@datasets[[k]]@datasettype, "pollen")
        } else {
          expect_equal(brazil_[[i]]@collunits[[j]]@datasets[[k]]@datasettype, "charcoal")
        }
      }
    }
  }
})


test_that("filter on datasettype runs as expected. count unique sites", {

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

  brazil_datasets <- get_datasets(loc = brazil[1])

  brazil_ <- neotoma2::filter(brazil_datasets,
                    datasettype == "pollen" | datasettype == "charcoal")

  brazil_ids <- length(unique(getids(brazil_)$siteid))
  brazil_length <- length(brazil_)

  expect_equal(brazil_ids, brazil_length)

})

test_that("filter on lat runs as expected. count unique sites", {

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

  brazil_datasets <- get_datasets(loc = brazil[1])

  brasil_space <- brazil_datasets %>% neotoma2::filter(lat > -18 & lat < -16)

  brazil_ids <- length(unique(getids(brasil_space)$siteid))
  brazil_length <- length(brasil_space)

  expect_equal(brazil_ids, brazil_length)

})

test_that("filtering by collunitid removes samples", {

  ## we don't want this to run on CRAN

  skip_on_cran()

  meer <- get_sites(sitename = "meerfeld%") %>% get_downloads()

  meerDS42510 <- meer %>% neotoma2::filter(collunitid == 29576)


  expect_true({samples(meerDS42510); TRUE})

})

test_that("filtering by datasetid keeps all other collection units
          where the dataset does not belong", {

  ## we don't want this to run on CRAN

  skip_on_cran()

  meer <- get_sites(sitename = "meerfeld%") %>%
    get_downloads()

  meerDS42510 <- meer %>%
    neotoma2::filter(datasetid == 42510)

  meerId <-  as.data.frame(collunits(meerDS42510))$collectionunitid

  expect_equal(meerId, 29576)

})


test_that("filter works regardless of before/after get_downloads", {

  core_sites <- c(13949,11904,13319,728,13248,2625,2806,13280,519,11745,273,
                  13956,11880,13321,9801,13698,11816,13909,13921)

  core_wc_dl_f <- get_sites(core_sites) %>%
    get_datasets() %>%
    get_downloads() %>%
    neotoma2::filter(datasettype == "water chemistry")

  core_wc_f_dl <- get_sites(core_sites) %>%
    get_datasets() %>%
    neotoma2::filter(datasettype == "water chemistry") %>%
    get_downloads()

  core_dl_f_ids <- getids(core_wc_dl_f)

  core_f_dl_ids <- getids(core_wc_f_dl)

  expect_setequal(core_dl_f_ids$siteid, core_f_dl_ids$siteid)
  expect_setequal(core_dl_f_ids$collunitid, core_f_dl_ids$collunitid)
  expect_setequal(core_dl_f_ids$datasetid, core_f_dl_ids$datasetid)

})

test_that("filter contains the right datasets/collunits I", {

  core_sites <- c(13949,11904,13319,728,13248,2625,2806,13280,519,11745,273,
                  13956,11880,13321,9801,13698,11816,13909,13921)

  core_wc_dl_f <- get_sites(core_sites) %>%
    get_datasets() %>%
    get_downloads() %>%
    neotoma2::filter(datasettype == "water chemistry")

  core_wc_f_dl <- get_sites(core_sites) %>%
    get_datasets() %>%
    neotoma2::filter(datasettype == "water chemistry") %>%
    get_downloads()

  site728a <- getids(core_wc_f_dl) %>% dplyr::filter(siteid == 728)
  expect_equal(site728a$collunitid, 30115)
  expect_equal(site728a$datasetid, 43118)


  site728b <- getids(core_wc_dl_f) %>% dplyr::filter(siteid == 728)
  expect_equal(site728b$collunitid, 30115)
  expect_equal(site728b$datasetid, 43118)

})

test_that("filter contains the right datasets/collunits II", {
  
  core_sites <- c(13949,11904,13319,728,13248,2625,2806,13280,519,11745,273,
                  13956,11880,13321,9801,13698,11816,13909,13921) 
  
  core_wc_dl_f <- get_sites(core_sites) %>%
    get_datasets() %>%
    get_downloads() %>%
    neotoma2::filter(datasettype == "water chemistry")
  
  core_wc_f_dl <- get_sites(core_sites) %>%
    get_datasets() %>%
    neotoma2::filter(datasettype == "water chemistry") %>%
    get_downloads()
  
  site11745a <- getids(core_wc_f_dl) %>% dplyr::filter(siteid == 11745)
  expect_setequal(site11745a$collunitid, c(13204,33232))
  expect_setequal(site11745a$datasetid, c(17706,46561))
  
  
  site11745b <- getids(core_wc_dl_f) %>% dplyr::filter(siteid == 11745)
  expect_setequal(site11745b$collunitid, c(13204,33232))
  expect_setequal(site11745b$datasetid, c(17706,46561))
  
})

