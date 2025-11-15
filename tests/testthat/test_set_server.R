library("testthat")
library("neotoma2")

context("Switching servers works between local, dev and neotoma APIs.")
testthat::test_that("`Switching server with `set_server()`.", {
  set_server("dev")
  testthat::expect_true(Sys.getenv("APIPOINT") == "dev")
  set_server("local")
  testthat::expect_true(Sys.getenv("APIPOINT") == "local")
  set_server("neotoma")
  testthat::expect_true(Sys.getenv("APIPOINT") == "neotoma")
})

testthat::test_that("Invalid server throws error.", {
  testthat::expect_error(set_server("invalid_server_name"))
})