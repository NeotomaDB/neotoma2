library("testthat")
library("neotoma2")

context("`cite_data()` function")
test_that("cite_data() returns citation objects", {
  skip_on_cran()
  st <- get_sites(c(24, 100))
  citation <- cite_data(st)
  testthat::expect_false(any(duplicated(df)))
  testthat::expect_is(df, "data.frame")
})