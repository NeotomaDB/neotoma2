library("testthat")
library("neotoma2")

context("`getids()` retrieves all 'site', 'collection unit', and
        'dataset' IDs from Neotoma2 objects")
test_that("getids gets correct site IDs", {
  skip_on_cran()
  sts <- get_sites(c(1001, 2001, 15, 24))
  ids <- getids(sts)
  testthat::expect_setequal(ids$siteid, c(1001, 2001, 15, 24))
})

test_that("getids gets correct dataset IDs", {
  skip_on_cran()
  dss <- get_datasets(c(1001, 2001, 15, 24))
  ids <- getids(dss)
  testthat::expect_setequal(ids$datasetid, c(1001, 2001, 15, 24))
})

test_that("getids gets correct IDs and correct alignment", {
  skip_on_cran()
  sts <- get_sites(c(1001, 2001, 15, 24))
  ids <- getids(sts)
  sts_ids_df <- purrr::map_df(sts@sites, function(x) {
    siteid <- x@siteid
    purrr::map(x@collunits, function(y) {
      cuid <- y@collectionunitid
      dsids <- purrr::map(y@datasets, function(z) z$datasetid) %>% unlist()
      # Return one row per datasetid
      data.frame(
        siteid           = siteid,
        collectionunitid = cuid,
        datasetid        = dsids
      )}) %>% 
      do.call(rbind, .)
  })
  testthat::expect_equal(nrow(ids), nrow(sts_ids_df))
  testthat::expect_setequal(ids$siteid, sts_ids_df$siteid)
  testthat::expect_setequal(ids$datasetid, sts_ids_df$datasetid)
  testthat::expect_setequal(ids$collunitid, sts_ids_df$collectionunitid)
  sts_cu <- sts_ids_df$collectionunitid %>% unique()
  st15_ids <- ids %>%
    dplyr::filter(siteid == 15)
  st15_map <- sts_ids_df %>%
    dplyr::filter(siteid == 15)
  testthat::expect_setequal(st15_ids$datasetid, st15_map$datasetid)
  testthat::expect_setequal(st15_ids$collunitid, st15_map$collectionunitid)
  st2001_ids <- ids %>%
    dplyr::filter(siteid == 2001)
  st2001_map <- sts_ids_df %>%
    dplyr::filter(siteid == 2001)
  testthat::expect_setequal(st2001_ids$datasetid, st2001_map$datasetid)
  testthat::expect_setequal(st2001_ids$collunitid, st2001_map$collectionunitid)
})