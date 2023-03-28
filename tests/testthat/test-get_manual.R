# tests for `get_manual()`.

test_that("Manual loading fails if interactive is false.", {
  expect_error(get_manual())
})

test_that("Manual loading succeeds when interactive is true:", {
  expect_null(rlang::with_interactive(get_manual(), value=TRUE))
})
