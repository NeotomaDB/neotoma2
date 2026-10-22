library("testthat")
library("neotoma2")

context("Working with general data from the database:")
test_that("Call a single database table:", {
  skip_on_cran()
  tb <- get_table("agetypes")
  testthat::expect_is(tb, "data.frame")
})

test_that("Apply limits for get_table()", {
  skip_on_cran()
  tb <- get_table("agetypes", limit = 1)
  testthat::expect_equal(nrow(tb), 1)
  tb <- get_table("agetypes", limit = 3)
  testthat::expect_equal(nrow(tb), 3)
})

test_that("Limit and offsets work as expected.
          Offsetting by one returns a different result.", {
            skip_on_cran()
            tb1 <- get_table("agetypes", limit = 1)
            tb2 <- get_table("agetypes", limit = 1, offset = 1)
            testthat::expect_false(rlang::hash(tb1) ==
                                     rlang::hash(tb2))
          })

test_that("Correct table", {
  skip_on_cran()
  tb1 <- get_table("agetypes", limit = 1)
  cols <- c("agetypeid", "agetype", "precedence",
            "shortagetype", "recdatecreated", "recdatemodified")
  testthat::expect_identical(colnames(tb1), cols)
})

## Offline tests: parseURL() is replaced, so these touch no API.

table_result <- function() {
  list(status = "success",
       data = list(data = list(list(agetypeid = 1, agetype = "A"),
                               list(agetypeid = 2, agetype = "B"))))
}

test_that("get_table asks the API once for the same table in one session", {
  neotoma2:::neotoma_cache_clear()
  on.exit(neotoma2:::neotoma_cache_clear())
  calls <- 0
  local_mocked_bindings(parseURL = function(...) {
    calls <<- calls + 1
    table_result()
  }, .package = "neotoma2")

  first <- get_table("agetypes")
  second <- get_table("agetypes")
  expect_equal(calls, 1)
  expect_identical(first, second)
  expect_equal(nrow(first), 2)

  # A different limit or offset is a different query.
  get_table("agetypes", limit = 1)
  get_table("agetypes", offset = 1)
  expect_equal(calls, 3)
})

test_that("get_table(refresh = TRUE) and the cache option ask again", {
  neotoma2:::neotoma_cache_clear()
  on.exit({
    neotoma2:::neotoma_cache_clear()
    options(neotoma2.cache = NULL)
  })
  calls <- 0
  local_mocked_bindings(parseURL = function(...) {
    calls <<- calls + 1
    table_result()
  }, .package = "neotoma2")

  get_table("agetypes")
  get_table("agetypes", refresh = TRUE)
  expect_equal(calls, 2)

  options(neotoma2.cache = FALSE)
  get_table("agetypes")
  get_table("agetypes")
  expect_equal(calls, 4)
})

test_that("a failed get_table call is not cached", {
  neotoma2:::neotoma_cache_clear()
  on.exit(neotoma2:::neotoma_cache_clear())
  calls <- 0
  local_mocked_bindings(parseURL = function(...) {
    calls <<- calls + 1
    stop("server busy")
  }, .package = "neotoma2")
  expect_message(first <- get_table("agetypes"), "API call failed")
  expect_null(first)
  suppressMessages(get_table("agetypes"))
  expect_equal(calls, 2)
})
