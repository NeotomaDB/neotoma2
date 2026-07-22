library("testthat")
library("neotoma2")

context("Verifying that neotoma objects do not have duplicates and
         are nested properly respecting Neotoma's data object:
        site <- cu <- ds")
test_that("Doubling a set of records results and cleaning results in a clean set.", {
  # c calls clean internally but we can call it again to be sure
  skip_on_cran()
  # Site 24 is one of the sites returned by the "Alex%" search, so combining
  # the two sets must not grow the set: `c()` cleans internally, and a second
  # explicit `clean()` must be a no-op rather than dropping or duplicating.
  alex <- get_sites(sitename = "Alex%")
  alex2 <- get_sites(24)
  alex_sets <- c(alex, alex2)
  clean_al <- clean(alex_sets, verbose = FALSE)
  testthat::expect_equal(length(alex_sets), length(alex))
  testthat::expect_equal(length(clean_al), length(alex_sets))
  # Read siteids off the site objects: `as.data.frame()` returns one row per
  # collection unit, so a multi-collunit site would look duplicated there.
  clean_ids <- vapply(clean_al@sites, function(s) as.numeric(s@siteid),
                      numeric(1))
  testthat::expect_false(any(duplicated(clean_ids)))
  fiftyds <- get_datasets(limit = 50)
  nextds <- get_datasets(limit = 50)
  doubled <- c(fiftyds, nextds)
  testthat::expect_equal(getids(doubled), getids(nextds))
  testthat::expect_equal(length(doubled), length(fiftyds))
})