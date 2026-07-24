library("testthat")
library("neotoma2")

context("get_publications tests")
test_that("`get_publications` numeric", {
  skip_on_cran()
  empty <- get_publications()
  testthat::expect_is(empty, "publications")
  # From numeric vector
  counts <- get_publications(c(1, 2, 3, 4))
  testthat::expect_is(counts, "publications")
  testthat::expect_is(counts[[1]], "publication")
  testthat::expect_length(counts, 4)
})

test_that("`get_publications` default", {
  skip_on_cran()
  pollen <- get_publications(search = "pollen")
  testthat::expect_is(pollen, "publications")
  testthat::expect_equal(length(pollen), 25)
})

test_that("`get_publications` publications", {
  skip_on_cran()
  counts <- get_publications(c(1, 2, 3, 4))
  frompubs <- get_publications(counts)
  testthat::expect_identical(counts, frompubs)
})