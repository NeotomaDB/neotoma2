library("testthat")
library("neotoma2")

context("Retrieves general Neotoma manual")
test_that("Manual loading fails if interactive is false.", {
  skip_on_cran()
  testthat::expect_error(get_manual())
})

test_that("Manual loading succeeds when interactive is true:", {
  skip_on_cran()
  testthat::expect_null(rlang::with_interactive(get_manual(),
                                                value=TRUE))
})