library("testthat")
library("neotoma2")

context("Ping Neotoma API to check availability")
test_that("The Pings work", {
  ping_head <- try(httr::HEAD("https://api.neotomadb.org"))
  ping_call <- try(pingNeotoma("neotoma"))
  testthat::expect_equal(ping_head$status_code,
    ping_call$status_code)
  
  ping_head <- try(httr::HEAD("https://api-dev.neotomadb.org"))
  ping_call <- try(pingNeotoma("dev"))
  testthat::expect_equal(ping_head$status_code,
                         ping_call$status_code)
})
