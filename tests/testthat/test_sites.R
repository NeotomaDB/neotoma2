## load packages
library("testthat")
library("neotoma2")

context("Run Neotoma `test_sites` only when not on CRAN")

test_that("get_sites runs as expected.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  alexander_lake <- get_sites(sitename = "Alexander Lake")
  
  # Check that sites' names are only Alexander Lake
  
  expect_identical(alexander_lake[[1]]@sitename, "Alexander Lake")
  
})

test_that("get_sites runs as expected.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  alex <- get_sites(sitename = "Alex%")
  
  # Check that sites' names are only Alexander Lake
  
  for (i in seq_len(length(alex))) {
    alex_char <- substring(alex[[i]]@sitename, 1,4)
    alex_char <- tolower(alex_char)
    expect_identical(alex_char, "alex")
  }
  
})

test_that("get_sites runs as expected.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  site.1001 <- get_sites(1001)
  
  # Check that sites' id matches 1001
  
  expect_equal(site.1001[[1]]@siteid, 1001)
  
})

test_that("get_sites runs as expected.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  sites.ob <- get_sites(c(1001, 2001, 15, 24))
  sites.vec <- getids(sites.ob)
  sites.vec <- unique(sites.vec$siteid)
  
  # Check that sites' id matches 1001

  expect_setequal(sites.vec, c(1001, 2001, 15, 24))
  
})

