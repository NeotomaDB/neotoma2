library("testthat")
library("neotoma2")

context("Test `parseURL()` transport helpers.")

test_that("neotoma_baseurl resolves servers and honors APIPOINT", {
  expect_equal(neotoma2:::neotoma_baseurl("neotoma"),
               "https://api.neotomadb.org/v2.0/")
  expect_equal(neotoma2:::neotoma_baseurl("dev"),
               "http://api-dev.neotomadb.org/v2.0/")
  expect_equal(neotoma2:::neotoma_baseurl("local"),
               "http://localhost:3001/v2.0/")
  old <- Sys.getenv("APIPOINT")
  Sys.setenv("APIPOINT" = "local")
  expect_equal(neotoma2:::neotoma_baseurl("neotoma"),
               "http://localhost:3001/v2.0/")
  if (old == "") Sys.unsetenv("APIPOINT") else Sys.setenv("APIPOINT" = old)
})

test_that("neotoma_id_param names the identifier from the endpoint path", {
  expect_equal(neotoma2:::neotoma_id_param("data/sites/1,2,3"), "siteid")
  expect_equal(neotoma2:::neotoma_id_param("data/datasets/1,2"), "datasetid")
  expect_equal(neotoma2:::neotoma_id_param("data/downloads?datasetid=5"),
               "datasetid")
})

test_that("neotoma_body builds a JSON body from the query", {
  body <- neotoma2:::neotoma_body(list(siteid = c(1, 2), limit = 50))
  expect_type(body, "character")
  expect_equal(as.character(body), "{\"siteid\":[1,2],\"limit\":50}")
})
