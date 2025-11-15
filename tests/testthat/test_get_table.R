library("testthat")
library("neotoma2")

context("Working with general data from the database:")
test_that("Call a single database table:", {
  tb <- get_table("agetypes")
  testthat::expect_is(tb, "data.frame")
})

test_that("Apply limits for get_table()", {
  tb <- get_table("agetypes", limit = 1)
  testthat::expect_equal(nrow(tb), 1)
  tb <- get_table("agetypes", limit = 3)
  testthat::expect_equal(nrow(tb), 3)
})

test_that("Limit and offsets work as expected.
          Offsetting by one returns a different result.", {
            tb1 <- get_table("agetypes", limit = 1)
            tb2 <- get_table("agetypes", limit = 1, offset = 1)
            testthat::expect_false(rlang::hash(tb1) ==
                                     rlang::hash(tb2))
          })

test_that("Correct table", {
            tb1 <- get_table("agetypes", limit = 1)
            cols <- c("agetypeid", "agetype", "precedence",
                      "shortagetype", "recdatecreated", "recdatemodified")
            testthat::expect_identical(colnames(tb1), cols)
          })