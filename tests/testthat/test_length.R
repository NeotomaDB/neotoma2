library("testthat")
library("neotoma2")

context("`length()` retrieves correct number of 'sites', 'collection units', and
        'datasets'")
test_that("length of sites is correct", {
  skip_on_cran()
  sts <- get_sites(c(1001, 2001, 15, 24))
  len <- length(sts)
  testthat::expect_equal(len, length(c(1001, 2001, 15, 24)))
})

test_that("length of collection units is correct", {
  skip_on_cran()
  sts <- get_sites(c(1001, 2001, 15, 24))
  cu <- collunits(sts)
  len <- length(cu)
  ids <- getids(sts)$collunitid %>% unique() %>% length()
  testthat::expect_equal(len, ids)
  len <- length(sts[[3]]@collunits)
  stid <- getids(sts[[3]])$siteid %>% unique()
  cuids <- filter(getids(sts), siteid == stid)$collunitid %>% length()
  testthat::expect_equal(len, cuids)
})

test_that("length of datasets is correct", {
  skip_on_cran()
  sts <- get_datasets(c(1001, 2001, 15, 24))
  ds <- datasets(sts)
  len <- length(ds)
  ids <- getids(sts)$datasetid %>% length()
  testthat::expect_equal(len, ids)
  len <- length(
    map(sts[[1]]@collunits, function(y) {
      map(y@datasets, function(z) z$datasetid) %>% unlist()
    }) %>% unlist()
  )
  stid <- getids(sts[[1]])$siteid %>% unique()
  dsids <- filter(getids(sts), siteid == stid)$datasetid %>% length()
  testthat::expect_equal(len, dsids)
})