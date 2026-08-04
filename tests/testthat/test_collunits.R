library("testthat")
library("neotoma2")
library("httptest")

httptest::with_mock_api({
test_that("collunits()`", {
  skip_on_cran()
  # Bounded to a few known sites so the recorded fixture stays small; the
  # accessor assertions below hold for any handful of sites.
  dls <- get_sites(c(24, 1001, 2001)) %>%
    get_downloads()
  sumDl <- summary(dls)
  ids <- getids(dls)
  cus <- collunits(dls)
  testthat::expect_identical(length(unique(cus$collectionunitid)),
                             length(unique(ids$collunitid)))
  testthat::expect_true(all(sumDl$collunit_name %in% cus$handle))
})
})

## Not every site has collection units -- the API returns some with none at all.
## `c()` used to fail on those, because `unlist()` returns NULL rather than an
## empty list when there is nothing to flatten, and the `collunits` slot must
## hold a list. That made `collunits()` unusable on any set containing such a
## site, since it reduces the per-site objects with `c()`.
test_that("collection units combine when some sites have none", {
  empty <- methods::new("collunits", collunits = list())
  one <- methods::new("collunits",
                      collunits = list(set_collunit(handle = "CU1")))
  two <- methods::new("collunits",
                      collunits = list(set_collunit(handle = "CU2")))

  testthat::expect_equal(length(c(empty, empty)), 0)
  testthat::expect_equal(length(c(empty, one)), 1)
  testthat::expect_equal(length(c(one, empty)), 1)
  testthat::expect_equal(length(c(one, two)), 2)
  # Combining a set with itself must still drop the duplicate.
  testthat::expect_equal(length(c(one, one)), 1)
})
