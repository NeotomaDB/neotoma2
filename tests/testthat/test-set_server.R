testthat::test_that("Switching Servers works between local, dev and neotoma.", {
  set_server("dev")
  expect_true(Sys.getenv("APIPOINT") == "dev")
  set_server("local")
  expect_true(Sys.getenv("APIPOINT") == "local")
  set_server("neotoma")
  expect_true(Sys.getenv("APIPOINT") == "neotoma")
})
