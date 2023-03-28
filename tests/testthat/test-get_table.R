context("Working with general data from the database:")
# Testing limits, offset and the base call for tables.
test_that("Call a single database table:", {

  # Make sure the call works:
  expect_is(get_table("agetypes"), "data.frame")
})

test_that("We can apply the limits for get_table()", {
  # Make sure a limit of 1 gives us a single row.
  expect_equal(nrow(get_table("agetypes", limit = 1)), 1)
})

test_that("Limit and offsets work for the get_table() call", {
  # Make sure that offsetting by one returns a different result.
  expect_false(rlang::hash(get_table("agetypes", limit = 1)) ==
    rlang::hash(get_table("agetypes", limit = 1, offset = 1)))
})
