testthat::skip("Skipping all tests in this file")
library("testthat")
library("neotoma2")

context("Creating speleothems from scratch or updating existing objects.")
test_that("`set_speleothem()` for a new publication", {
  skip_on_cran()
  new_speleo <- set_speleothem(
    entityid = 56,
    entityname = "My Speleothem",
    siteid = 54,
    collectionunitid = 32
  )
  testthat::expect_is(new_speleo, "speleothem")
  testthat::expect_equal(new_speleo@entityid, 56)
  testthat::expect_equal(new_speleo@entityname, "My Speleothem")
  testthat::expect_equal(new_speleo@siteid, 54)
  testthat::expect_equal(new_speleo@collectionunitid, 32)
})

test_that("Updating existing publication", {
  skip_on_cran()
  new_speleo <- set_speleothem(
    entityid = 56,
    entityname = "My Speleothem",
    siteid = 54,
    collectionunitid = 32
  )
  site <- get_sites(3)
  siteid <- site[[1]]@siteid
  site[[1]]@collunits[[1]]@speleothems <- new("speleothems",
                                              speleothems = list(new_speleo))

  testthat::expect_error(set_speleothems(x = site, siteid = siteid))
  testthat::expect_is(new_speleo, "speleothem")
  speleo <- site[[1]]@collunits[[1]]@speleothems[[1]]
  updated_speleo <- set_speleothem(x = speleo, siteid = siteid)
  testthat::expect_equal(updated_speleo@siteid, site[[1]]@siteid)
})
