testthat::skip("Skipping all tests in this file")
library("testthat")
library("neotoma2")

context("Creating speleothems from scratch or updating existing objects.")
test_that("`set_speleothem()` for a new publication", {
  skip_on_cran()
  newSpeleo <- set_speleothem(entityid = 56,
                           entityname = "My Speleothem",
                           siteid = 54,
                           collectionunitid = 32)
  testthat::expect_is(newSpeleo, "speleothem")
  testthat::expect_equal(newSpeleo@entityid, 56)
  testthat::expect_equal(newSpeleo@entityname, "My Speleothem")
  testthat::expect_equal(newSpeleo@siteid, 54)
  testthat::expect_equal(newSpeleo@collectionunitid, 32)
})

test_that("Updating existing publication", {
  skip_on_cran()
  newSpeleo <- set_speleothem(entityid = 56,
                              entityname = "My Speleothem",
                              siteid = 54,
                              collectionunitid = 32)
  site <- get_sites(3)
  siteid <- site[[1]]@siteid
  site[[1]]@collunits[[1]]@speleothems <- new("speleothems", speleothems = list(newSpeleo))
  
  testthat::expect_error(set_speleothems(x = site, siteid = siteid))
  testthat::expect_is(newSpeleo, "speleothem")
  updatedSpeleo <- set_speleothem(x = site[[1]]@collunits[[1]]@speleothems[[1]], siteid = siteid)
  testthat::expect_equal(updatedSpeleo@siteid, site[[1]]@siteid)
})