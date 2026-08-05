library("testthat")
library("neotoma2")

context("Test `parse_speleothem()` against partial API records.")

## These run offline against hand-made payloads, so they need no API and no
## skip. Note that test_get_speleothems.R is disabled by a file-level skip(),
## which is why the regression below went unnoticed.

## The shape parse_speleothem() consumes: a list of records, each wrapping the
## speleothem fields under `$speleothem`.
speleothem_record <- function(entityid, ...) {
  fields <- list(entityid = entityid,
                 entityname = paste0("Entity ", entityid),
                 siteid = 1,
                 collectionunitid = 1,
                 datasetid = 1)
  list(speleothem = utils::modifyList(fields, list(...)))
}

test_that("a record without entrancedistanceunits parses", {
  # The API omits this field rather than sending null, so `x$entrancedistanceunits`
  # is NULL and `NULL == 37` is logical(0) -- which `if` cannot evaluate. Three of
  # the seven La Vallina records come back this way, so an unguarded comparison
  # takes down get_speleothems() for most real queries.
  result <- neotoma2:::parse_speleothem(list(speleothem_record(1)))

  testthat::expect_s4_class(result, "speleothems")
  testthat::expect_length(result@speleothems, 1)
  testthat::expect_true(is.na(result@speleothems[[1]]@entrancedistanceunits))
})

test_that("the unit code 37 is still translated to metres", {
  result <- neotoma2:::parse_speleothem(
    list(speleothem_record(2, entrancedistanceunits = 37)))

  testthat::expect_equal(result@speleothems[[1]]@entrancedistanceunits, "m")
})

## Note: the API only ever sends this field as the code 37 or omits it, which is
## what these tests cover. A different numeric code would still fail, in the slot
## assignment rather than here -- out of scope for this fix.

test_that("a missing measurement does not pick up its own units string", {
  # `$` on a list falls back to partial matching, so reading an absent
  # `entrancedistance` used to return `entrancedistanceunits` instead and write
  # that string into a numeric slot. Same trap for dripheight/dripheightunits.
  result <- neotoma2:::parse_speleothem(
    list(speleothem_record(4,
                           entrancedistanceunits = 37,
                           dripheightunits = "cm")))
  speleothem <- result@speleothems[[1]]

  testthat::expect_true(is.na(speleothem@entrancedistance))
  testthat::expect_true(is.na(speleothem@dripheight))
  testthat::expect_equal(speleothem@entrancedistanceunits, "m")
  testthat::expect_equal(speleothem@dripheightunits, "cm")
})

test_that("a mixed batch parses every record", {
  # The real failure mode: one good record and one missing the field. Before the
  # guard the whole batch errored, losing the valid entries too.
  result <- neotoma2:::parse_speleothem(
    list(speleothem_record(1),
         speleothem_record(2, entrancedistanceunits = 37),
         speleothem_record(3)))

  testthat::expect_length(result@speleothems, 3)
  units <- vapply(result@speleothems,
                  function(x) as.character(x@entrancedistanceunits),
                  character(1))
  testthat::expect_equal(sum(is.na(units)), 2)
  testthat::expect_equal(sum(units == "m", na.rm = TRUE), 1)
})
