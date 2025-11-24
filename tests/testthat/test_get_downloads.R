library("testthat")
library("neotoma2")

context("get_downloads() retrieves data from /download endpoint 
        and parses content as expected.")
test_that("get_downloads numeric", {
  skip_on_cran()
  download_1001 <- neotoma2::get_downloads(1001)
  datasets_ids <- neotoma2::getids(download_1001) %>%
    dplyr::select(datasetid) %>%
    unique()
  # Downloads using dsid, verify that we only have
  # objects belonging to that dsid
  testthat::expect_equivalent(datasets_ids, 1001)
})

test_that("get_downloads from get_datasets()
          sites object.", {
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
            brazil_datasets <- get_datasets(loc = brazil_sf, all_data = TRUE)
            brazil_dl <- get_downloads(brazil_datasets)
            testthat::expect_identical(nrow(getids(brazil_datasets)),
                                       nrow(getids(brazil_dl)))
            testthat::expect_equal(getids(brazil_datasets), 
                                   getids(brazil_dl))
          })

test_that("get_downloads from get_sites sites", {
  skip_on_cran()
  core_sites <- c(13949, 11904, 13319, 728,
                  13248, 2625, 2806, 13280, 519, 11745,
                  273, 13956, 11880, 13321, 9801, 13698,
                  11816, 13909, 13921)
  df1 <- get_sites(core_sites) %>%
    get_downloads() %>%
    getids()
  df2 <- get_sites(core_sites) %>%
    getids()
  # The get_downloads limit happens, so we have fewer rows in df1
  testthat::expect_lte(nrow(df1), nrow(df2))
  testthat::expect_true(all(unique(df1$datasetid) %in% unique(df2$datasetid)))
})

test_that("Faunmap dataset", {
  mydataset <- get_downloads(7032)
  testthat::expect_is(mydataset, "sites")
})

test_that("get_downloads with or without all_data works.", {
  skip_on_cran()
  uk_bbox_geojson <- "{\n\"type\": \"FeatureCollection\",\n\"name\": \"out\",\n\"crs\": { \"type\": \"name\", \"properties\": { \"name\": \"urn:ogc:def:crs:OGC:1.3:CRS84\" } },\n\"features\": [\n{ \"type\": \"Feature\", \"properties\": { }, \"geometry\": { \"type\": \"Polygon\", \"coordinates\": [ [ [ -10.390234374999977, 50.021386718749994 ], [ 1.74658203125, 50.021386718749994 ], [ 1.74658203125, 60.831884765624991 ], [ -10.390234374999977, 60.831884765624991 ], [ -10.390234374999977, 50.021386718749994 ] ] ] } }\n]\n}"
  uk_datasets <- get_datasets(
    loc = uk_bbox_geojson,
    datasettype = 'pollen',
    limit=5
  )
  uk_dl <- get_downloads(uk_datasets, all_data=TRUE)
  uk_dl2 <- get_downloads(uk_datasets)
  testthat::expect_equal(nrow(as.data.frame(datasets(uk_dl))), 
                         nrow(as.data.frame(datasets(uk_dl2))))
  # expect no error
  testthat::expect_error(get_downloads(uk_datasets, all_data=TRUE), NA)
  testthat::expect_error(get_downloads(uk_datasets, all_data=FALSE), NA)
  testthat::expect_error(get_downloads(uk_datasets), NA)
})

test_that("get_downloads handles empty result", {
  skip_on_cran()
  gpids <- c(7326, 6442, 7923, 7990, 7368, 8480, 8981, 7934)
  ne_sites <- c() 
  for (id in gpids) {
    search_1 <- get_sites(gpid = id)
    ne_sites <- c(ne_sites, search_1)
  }
  diatom_list <- c("diatom", "diatom top-bottom", "diatom bottom sample")
  bottom_diat_info <- c()
  for (type in diatom_list) {
    ne_sites 
    bottom_diatoms <- c(bottom_diat_info, ne_sites) 
  } 
  bottom_diatoms
  testthat::expect_error(get_downloads(bottom_diatoms), NA)
})
