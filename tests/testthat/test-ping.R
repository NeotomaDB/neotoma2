## load packages
test_that("The Pings work", {
  ping_head <- try(httr::HEAD("https://api.neotomadb.org"))
  ping_call <- try(pingNeotoma("neotoma"))
  testthat::expect_equal(ping_head$status_code,
    ping_call$status_code)
})
