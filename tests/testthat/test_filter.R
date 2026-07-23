testthat::skip("Skipping all tests in this file")
library("testthat")
library("neotoma2")

context("Test that filter receives a sites object
        and filters using dplyr's syntax")
test_that("filter datasettype", {
  skip_on_cran()
  sts <- get_sites()
  pollen_sts <-
    sts %>%
    neotoma2::filter(datasettype == "pollen")
  sum <- summary(pollen_sts)
  pollen_types <- unique(sum$dataset_types)
  testthat::expect_equal(pollen_types, "pollen")
})

test_that("filter datasettype + loc", {
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
  brazil_datasets <- get_datasets(loc = brazil[1], limit=10)
  brazil_pollen <- neotoma2::filter(brazil_datasets, datasettype == "pollen")
  brazil_summary <- summary(brazil_pollen)
  brazil_datatype <- unique(brazil_summary$dataset_types)
  testthat::expect_equal(brazil_datatype, "pollen")
})

test_that("filter lat & long.", {
  skip_on_cran()
  sts <- get_sites()
  lat_sts <-
    sts %>%
    filter(lat >= 50 & lat <= 90)
  latitudes <- as.data.frame(lat_sts)$lat
  testthat::expect_true(all(latitudes <= 90))
  testthat::expect_true(all(latitudes >= 50))
  testthat::expect_lt(length(lat_sts), length(sts))
  testthat::expect_true(all(getids(lat_sts)$siteid %in% getids(sts)$siteid))
  testthat::expect_true(all(getids(lat_sts)$datasetid %in% getids(sts)$datasetid))
  testthat::expect_true(all(getids(lat_sts)$collunitid %in% getids(sts)$collunitid))
})

test_that("filter get_datasets('datasettype') by datasettype
          does not change sites", {
            skip_on_cran()
            ds <- get_datasets(datasettype = "pollen")
            pollen_ds <- neotoma2::filter(ds, datasettype == "pollen")
            testthat::expect_equal(length(ds), length(pollen_ds))
            # test ids in datastid for ds and pollen ds are the same
            testthat::expect_setequal(getids(ds)$datasetid,
                                      getids(pollen_ds)$datasetid)
          })

test_that("filter by 2 datasettypes", {
  skip_on_cran()
  sts <- get_sites()
  filtered <- sts %>%
    neotoma2::filter(datasettype == "pollen" | datasettype == "charcoal")
  dst <- datasets(filtered) %>%
    as.data.frame() %>%
    select(datasettype) %>%
    unlist()
  testthat::expect_true(all(dst %in% c("pollen", "charcoal")))
})

test_that("filter datasets returns a clean object", {
  skip_on_cran()
  sts <- get_sites() %>%
    neotoma2::filter(datasettype == "pollen" | datasettype == "charcoal")
  ids <- getids(sts) %>%
    select(siteid) %>%
    unique() %>%
    nrow()
  testthat::expect_equal(ids, length(sts))
  testthat::expect_lte(length(sts), nrow(summary(sts)))
})

test_that("filter by collunitid removes samples", {
  skip_on_cran()
  meer <- get_sites(sitename = "meerfeld%") %>%
    get_downloads()
  meerDS42510 <- meer %>%
    neotoma2::filter(collunitid == 29576) %>%
    samples()
  testthat::expect_true(all(meerDS42510$collectionunitid == 29576))
})

test_that("filter by datasetid keeps all other collection units
           where the dataset does not belong", {
             skip_on_cran()
             meer <- get_sites(sitename = "meerfeld%") %>%
               get_downloads()
             meerDS42510 <- meer %>%
               neotoma2::filter(datasetid == 42510)
             meerId <-  as.data.frame(collunits(meerDS42510))$collectionunitid
             testthat::expect_equal(meerId, 29576)
           })

test_that("multi-condition filter across two levels (regression)", {
  skip_on_cran()
  sites <- get_sites(limit = 10) %>% get_datasets()
  result <- sites %>% filter(altitude < 3000, datasettype == "pollen")
  testthat::expect_s4_class(result, "sites")
  dts <- as.data.frame(datasets(result))
  testthat::expect_true(all(dts$datasettype == "pollen"))
})

test_that("order of conditions does not matter", {
  skip_on_cran()
  sites <- get_sites(limit = 10) %>% get_datasets()
  a <- sites %>% filter(altitude < 3000, datasettype == "pollen")
  b <- sites %>% filter(datasettype == "pollen", altitude < 3000)
  testthat::expect_equal(length(a@sites), length(b@sites))
})

test_that("single-condition filters still work (no regression)", {
  skip_on_cran()
  sites <- get_sites(limit = 10) %>% get_datasets()
  s_site <- sites %>% filter(altitude < 3000)
  s_ds   <- sites %>% filter(datasettype == "pollen")
  testthat::expect_s4_class(s_site, "sites")
  testthat::expect_s4_class(s_ds, "sites")
})

test_that("three levels at once", {
  skip_on_cran()
  sites <- get_sites(limit = 10) %>% get_datasets()
  result <- sites %>%
    filter(altitude < 5000, datasettype == "pollen", waterdepth > 0)
  testthat::expect_s4_class(result, "sites")
})

test_that("substring false-positive guard (value contains a column substring)", {
  skip_on_cran()
  sites <- get_sites(limit = 10) %>% get_datasets()
  # "LONGCORE" contains "long", but the referenced column is `handle`
  # (a collunit column), so only the collunit join should be triggered.
  result <- sites %>% filter(handle == "SOME-LONG-HANDLE")
  testthat::expect_s4_class(result, "sites")
})

test_that("no matches returns an empty sites", {
  skip_on_cran()
  sites <- get_sites(limit = 10) %>% get_datasets()
  result <- sites %>% filter(altitude < -100000)
  testthat::expect_s4_class(result, "sites")
  testthat::expect_equal(length(result@sites), 0)
})

test_that("filter(NULL) warns and returns NULL", {
  testthat::expect_warning(res <- filter(NULL), "No sites to filter")
  testthat::expect_null(res)
})

test_that("spatial loc filter keeps only sites inside the region", {
  skip_on_cran()
  brazil <- paste0('{"type": "Polygon", "coordinates": [[',
                   '[-73.125, -9.102], [-56.953, -33.138],',
                   '[-36.563, -7.711], [-68.203, 13.923],',
                   '[-73.125, -9.102]]]}')
  brazil_sf <- geojsonsf::geojson_sf(brazil)
  sites <- get_datasets(loc = brazil, limit = 20)
  result <- sites %>% filter(loc == brazil_sf)
  testthat::expect_s4_class(result, "sites")
  testthat::expect_lte(length(result@sites), length(sites))
})

test_that("loc combines with an ordinary condition", {
  skip_on_cran()
  brazil <- paste0('{"type": "Polygon", "coordinates": [[',
                   '[-73.125, -9.102], [-56.953, -33.138],',
                   '[-36.563, -7.711], [-68.203, 13.923],',
                   '[-73.125, -9.102]]]}')
  brazil_sf <- geojsonsf::geojson_sf(brazil)
  sites <- get_datasets(loc = brazil, limit = 20)
  result <- sites %>% filter(altitude < 3000, loc == brazil_sf)
  testthat::expect_s4_class(result, "sites")
  elev <- as.data.frame(result)$elev
  testthat::expect_true(all(elev < 3000 | is.na(elev)))
})

test_that("geography is an alias for loc", {
  skip_on_cran()
  brazil <- paste0('{"type": "Polygon", "coordinates": [[',
                   '[-73.125, -9.102], [-56.953, -33.138],',
                   '[-36.563, -7.711], [-68.203, 13.923],',
                   '[-73.125, -9.102]]]}')
  brazil_sf <- geojsonsf::geojson_sf(brazil)
  sites <- get_datasets(loc = brazil, limit = 20)
  a <- sites %>% filter(loc == brazil_sf)
  b <- sites %>% filter(geography == brazil_sf)
  testthat::expect_equal(length(a@sites), length(b@sites))
})

test_that("filter(loc == WKT) matches filter(loc == GeoJSON)", {
  skip_on_cran()
  brazil_geojson <- paste0('{"type": "Polygon", "coordinates": [[',
                           '[-73.125, -9.102], [-56.953, -33.138],',
                           '[-36.563, -7.711], [-68.203, 13.923],',
                           '[-73.125, -9.102]]]}')
  brazil_wkt <- paste0("POLYGON ((-73.125 -9.102, -56.953 -33.138, ",
                       "-36.563 -7.711, -68.203 13.923, -73.125 -9.102))")
  sites <- get_datasets(loc = brazil_geojson, limit = 20)
  a <- sites %>% filter(loc == brazil_geojson)
  b <- sites %>% filter(loc == brazil_wkt)
  testthat::expect_s4_class(b, "sites")
  testthat::expect_equal(length(a@sites), length(b@sites))
})

test_that("WKT loc combines with an ordinary condition (geography alias)", {
  skip_on_cran()
  brazil_geojson <- paste0('{"type": "Polygon", "coordinates": [[',
                           '[-73.125, -9.102], [-56.953, -33.138],',
                           '[-36.563, -7.711], [-68.203, 13.923],',
                           '[-73.125, -9.102]]]}')
  brazil_wkt <- paste0("POLYGON ((-73.125 -9.102, -56.953 -33.138, ",
                       "-36.563 -7.711, -68.203 13.923, -73.125 -9.102))")
  sites <- get_datasets(loc = brazil_geojson, limit = 20)
  r <- sites %>% filter(altitude < 3000, geography == brazil_wkt)
  testthat::expect_s4_class(r, "sites")
  elev <- as.data.frame(r)$elev
  testthat::expect_true(all(elev < 3000 | is.na(elev)))
})

test_that("filter works before/after get_downloads", {
  skip_on_cran()
  core_sites <- c(13949, 11904, 13319, 728, 
                  13248, 2625, 2806,
                  13280, 519, 11745, 273, 13956,
                  11880, 13321, 9801, 13698, 11816,
                  13909, 13921)
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
  testthat::expect_setequal(core_dl_f_ids$siteid, core_f_dl_ids$siteid)
  testthat::expect_setequal(core_dl_f_ids$collunitid, core_f_dl_ids$collunitid)
  testthat::expect_setequal(core_dl_f_ids$datasetid, core_f_dl_ids$datasetid)
})