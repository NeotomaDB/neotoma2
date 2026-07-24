## load packages
library("testthat")
library("neotoma2")
library("httptest")

context("Run Neotoma `test_sites` only when not on CRAN")
httptest::with_mock_api({
  test_that("get_sites runs as expected.", {
    skip_on_cran()
    alexander_lake <- get_sites(sitename = "Alexander Lake")
    testthat::expect_identical(alexander_lake[[1]]@sitename, "Alexander Lake")
  })
  
  test_that("get_sites runs as expected.", {
    skip_on_cran()
    alex <- get_sites(sitename = "Alex%")
    for (i in seq_len(length(alex))) {
      alex_char <- substring(alex[[i]]@sitename, 1,4)
      alex_char <- tolower(alex_char)
      testthat::expect_identical(alex_char, "alex")
    }
  })
  
  test_that("get_sites runs as expected.", {
    skip_on_cran()
    site.1001 <- get_sites(1001)
    testthat::expect_equal(site.1001[[1]]@siteid, 1001)
  })
  
  test_that("get_sites runs as expected.", {
    skip_on_cran()
    sites.ob <- get_sites(c(1001, 2001, 15, 24))
    sites.vec <- getids(sites.ob)
    sites.vec <- unique(sites.vec$siteid)
    testthat::expect_setequal(sites.vec, c(1001, 2001, 15, 24))
  })
  
  
  test_that("count() counts sites correctly.", {
    skip_on_cran()
    sites.ob <- get_sites(c(1001, 2001, 15, 24))
    length_sites = length(sites.ob)
    count_sites = count(sites.ob)
    number_collunit_names = summary(sites.ob) %>% distinct(collunit_name) %>% dplyr::count()
    count_collunits = count(sites.ob,level="collunits")
    count_datasets = count(sites.ob,level="datasets")
    sum_datasets = sum(summary(sites.ob)$n_datasets)
    testthat::expect_equal(number_collunit_names$n[[1]],count_collunits)
    testthat::expect_equal(sum_datasets,count_datasets)
    testthat::expect_equal(length_sites,count_sites)
  })
  
  # test_that("All Czech sites work with different spatial bounds:", {
  #   skip_on_cran()
  #   cz_json <- '{"type": "Polygon",
  #         "coordinates": [[
  #             [12.40, 50.14],
  #             [14.10, 48.64],
  #             [16.95, 48.66],
  #             [18.91, 49.61],
  #             [15.24, 50.99],
  #             [12.40, 50.14]]]}'
  #   cz_WKT <- 'POLYGON ((12.4 50.14,
  #                        14.1 48.64,
  #                        16.95 48.66,
  #                        18.91 49.61,
  #                        15.24 50.99,
  #                        12.4 50.14))'
  #   cz_bbox <- c(12.4, 48.64, 18.91, 50.99)
  # 
  #   testthat::expect_true(all.equal(get_sites(loc=cz_json),
  #     get_sites(loc=cz_WKT)))
  # 
  #   # Now, we know that all sites in cz_sites[[1]] should be in cz_sites[[3]],
  #   # but the bounding box strategy means that the reverse is not true:
  #   cz_ids <- getids(get_sites(loc=cz_json[1]))
  #   testthat::expect_true(all(cz_ids$siteid %in%
  #     getids(get_sites(loc = cz_bbox, limit = 50))$siteid))
  # })
  
  test_that("All Data + loc work", {
    skip_on_cran()
    on.exit(Sys.sleep(10), add = TRUE)
    europe_json <- '{"type": "Polygon",
        "coordinates": [
          [[-23.5546875, 70.8446726342528],
           [-25.3125, 39.36827914916014],
           [61.87499999999999, 39.36827914916014],
           [62.57812500000001, 74.01954331150228],
           [-23.5546875, 70.8446726342528]]
        ]}'
    # This only needs to prove that a large region returns many sites; a single
    # page (`limit = 100`) satisfies `> 50` without paginating the whole continent.
    data <- get_sites(loc = europe_json[1], limit = 100)
    testthat::expect_gt(length(data), 50)
    
    # Now, we know that all sites in cz_sites[[1]] should be in cz_sites[[3]],
    # but the bounding box strategy means that the reverse is not true:
  })
})
