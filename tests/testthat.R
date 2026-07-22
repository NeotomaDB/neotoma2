library(testthat)
library(neotoma2)
library(httptest)

# Set working directory to package root for httptest
pkg_dir <- find.package("neotoma2")
withr::with_dir(pkg_dir, {
  httptest::use_httptest()
})

test_check("neotoma2")
