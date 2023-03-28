# Testing limits, offset and the base call for tables.
test_that("Call a database table:", {

  # Make sure the call works:
  expect_is(get_table("agetypes"), "data.frame")

  # Make sure a limit of 1 gives us a single row.
  expect_equal(nrow(get_table("agetypes", limit = 1)), 1)

  # Make sure that offsetting by one returns a different result.
  expect_false(hash(get_table("agetypes", limit = 1)) ==
    hash(get_table("agetypes", limit = 1, offset = 1)))
})
