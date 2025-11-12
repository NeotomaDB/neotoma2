library("testthat")
library("neotoma2")

context("Verifying that neotoma objects do not have duplicates and
         are nested properly respecting Neotoma's data object:
        site <- cu <- ds")
test_that("Doubling a set of records results and cleaning results in a clean set.", {
  # c calls clean internally but we can call it again to be sure
  skip_on_cran()
  alex <- get_sites(sitename = "Alex%")
  alex2 <- get_sites(24)
  alex_sets <- c(alex, alex2)
  clean_al <- clean(alex_sets)
  fiftyds <- get_datasets(limit = 50)
  nextds <- get_datasets(limit = 50)
  doubled <- c(fiftyds, nextds)
  testthat::expect_equal(getids(doubled), getids(nextds))
  testthat::expect_equal(length(doubled), length(fiftyds))
})