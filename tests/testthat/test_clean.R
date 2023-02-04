test_that("Doubling a set of records results and cleaning results in a clean set.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  fiftyds <- get_datasets(limit = 50)
  nextds <- get_datasets(limit = 50)

  doubled <- c(fiftyds, nextds)
  cleaned <- clean(doubled)
  testthat::expect_equivalent(getids(fiftyds), getids(cleaned))
})