testthat::test_that("Switching Servers works between local, dev and neotoma.", {
  set_server("dev")
  testthat::expect_true(Sys.getenv("APIPOINT") == "dev")
  set_server("local")
  testthat::expect_true(Sys.getenv("APIPOINT") == "local")
  set_server("neotoma")
  testthat::expect_true(Sys.getenv("APIPOINT") == "neotoma")
})
