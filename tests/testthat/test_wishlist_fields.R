library("testthat")
library("neotoma2")

# These tests build objects from hand-made API-style lists, so they run offline.
# The `collectors`, `analysisdate`, `samplenotes` and `analysisunitnotes` keys
# only reach the live API once the database functions `doi.datasetinfo` and
# `doi.ndbdata` are redeployed.

context("Site area, collection unit GPS and collectors, sample notes, dates.")

test_that("`set_site()` keeps the site area", {
  site <- set_site(sitename = "My Lake", area = 12.5)
  testthat::expect_equal(site@area, 12.5)
  updated <- set_site(x = site, area = 30)
  testthat::expect_equal(updated@area, 30)
})

test_that("`gpslocation` becomes an `sf` point with X and Y", {
  cu <- neotoma2:::build_collunits(list(
    collectionunitid = 7,
    gpslocation = list(
      latitude = 43.07, longitude = -89.41,
      gpsaltitude = 260, gpserror = 5
    )
  ))
  testthat::expect_s3_class(cu@gpslocation, "sf")
  xy <- sf::st_coordinates(cu@gpslocation)
  testthat::expect_equal(unname(xy[, "X"]), -89.41)
  testthat::expect_equal(unname(xy[, "Y"]), 43.07)
  testthat::expect_equal(cu@gpslocation$gpsaltitude, 260)
  testthat::expect_equal(sf::st_crs(cu@gpslocation)$epsg, 4326L)
})

test_that("A missing GPS fix gives an empty `sf` object", {
  empty <- list(
    list(
      latitude = NULL, longitude = NULL,
      gpsaltitude = NULL, gpserror = NULL
    ),
    list(latitude = 43.07, longitude = NULL),
    NULL
  )
  for (gps in empty) {
    cu <- neotoma2:::build_collunits(list(
      collectionunitid = 7,
      gpslocation = gps
    ))
    testthat::expect_s3_class(cu@gpslocation, "sf")
    testthat::expect_equal(nrow(cu@gpslocation), 0)
  }
})

test_that("Collectors are read as an ordered list of contact names", {
  cu <- neotoma2:::build_collunits(list(
    collectionunitid = 7,
    collectors = list(
      list(contactid = 1, contactname = "Hanenberger, Ned"),
      list(contactid = 2, contactname = "Watts, Jane")
    )
  ))
  testthat::expect_equal(
    unlist(cu@collectors),
    c("Hanenberger, Ned", "Watts, Jane")
  )
  # The API returns null when nobody is recorded, and older servers omit it.
  none <- neotoma2:::build_collunits(list(
    collectionunitid = 7,
    collectors = NULL
  ))
  testthat::expect_equal(length(none@collectors), 0)
})

test_that("`set_collunit()` accepts collectors", {
  cu <- set_collunit(collectors = list("Watts, Jane"))
  testthat::expect_equal(unlist(cu@collectors), "Watts, Jane")
  updated <- set_collunit(x = cu, collectors = list("A", "B"))
  testthat::expect_equal(length(updated@collectors), 2)
})

test_that("Sample notes, analysis date and analysis unit notes are read", {
  sm <- neotoma2:::build_sample(list(
    sampleid = 11,
    analysisunitid = 3,
    analysisunitnotes = "Depths measured from bottom up.",
    analysisdate = "1978-01-01",
    samplenotes = "Re-counted"
  ))
  testthat::expect_equal(
    sm@analysisunitnotes,
    "Depths measured from bottom up."
  )
  testthat::expect_equal(sm@analysisdate, as.Date("1978-01-01"))
  testthat::expect_s3_class(sm@analysisdate, "Date")
  testthat::expect_equal(sm@samplenotes, "Re-counted")
})

test_that("Missing sample notes and dates are typed NA", {
  sm <- neotoma2:::build_sample(list(sampleid = 11, analysisdate = NULL))
  testthat::expect_true(is.na(sm@analysisunitnotes))
  testthat::expect_true(is.na(sm@analysisdate))
  testthat::expect_s3_class(sm@analysisdate, "Date")
  testthat::expect_true(is.na(sm@samplenotes))
})

test_that("`set_sample()` accepts the new fields", {
  sm <- set_sample(
    sampleid = 42,
    analysisdate = as.Date("2020-05-17"),
    samplenotes = "note",
    analysisunitnotes = "au note"
  )
  testthat::expect_equal(sm@analysisdate, as.Date("2020-05-17"))
  testthat::expect_equal(sm@samplenotes, "note")
  testthat::expect_equal(sm@analysisunitnotes, "au note")
  updated <- set_sample(x = sm, samplenotes = "changed")
  testthat::expect_equal(updated@samplenotes, "changed")
  testthat::expect_error(set_sample(x = sm, analysisdate = "not a date"))
})
