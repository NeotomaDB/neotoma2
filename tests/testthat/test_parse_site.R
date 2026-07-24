library("testthat")
library("neotoma2")

context("Test `parse_site()` builds the right number of objects.")

test_that("parse_site keeps one site, one collection unit and both datasets", {
  result <- jsonlite::fromJSON(test_path("fixtures", "data_sites24.json"),
                               simplifyVector = FALSE)
  sites <- neotoma2:::parse_site(result)
  ids <- getids(sites)
  # The API returns two collection units that share id 24 but each carries a
  # different dataset (24 and 7870). After parsing we keep a single collection
  # unit that holds both datasets.
  testthat::expect_length(sites, 1)
  testthat::expect_equal(length(unique(ids$collunitid)), 1)
  testthat::expect_setequal(ids$datasetid, c(24, 7870))
})

test_that("parse_site of a datasets response keeps the site and dataset", {
  result <- jsonlite::fromJSON(test_path("fixtures", "data_datasets24.json"),
                               simplifyVector = FALSE)
  sites <- neotoma2:::parse_site(result)
  ids <- getids(sites)
  testthat::expect_length(sites, 1)
  testthat::expect_equal(length(unique(ids$collunitid)), 1)
  testthat::expect_setequal(ids$datasetid, 24)
})

test_that("parse_site of a downloads response folds the repeated unit", {
  result <- jsonlite::fromJSON(test_path("fixtures", "data_downloads24_dup.json"),
                               simplifyVector = FALSE)
  # `downloads` returns one element per dataset, so collection unit 24 arrives
  # twice, once for dataset 24 and once for dataset 7870. We keep one unit.
  testthat::expect_length(result$data, 2)
  sites <- neotoma2:::parse_site(result)
  ids <- getids(sites)
  testthat::expect_length(sites, 1)
  testthat::expect_equal(length(unique(ids$collunitid)), 1)
  testthat::expect_setequal(ids$datasetid, c(24, 7870))
})

test_that("parse_site reads the same site from every endpoint format", {
  # The three endpoints describe site 24 in three different shapes, and spell
  # several fields differently. They should still parse to the same site.
  files <- c("data_sites24.json", "data_datasets24.json",
             "data_downloads24_dup.json")
  sites <- lapply(files, function(f) {
    neotoma2:::parse_site(jsonlite::fromJSON(test_path("fixtures", f),
                                             simplifyVector = FALSE))
  })
  for (site in sites) {
    testthat::expect_length(site, 1)
    testthat::expect_equal(site[[1]]@siteid, 24)
    testthat::expect_equal(site[[1]]@sitename, "A Place in Neotoma")
    # `sitedescription` used to be dropped on the way to `build_site()`.
    testthat::expect_true(!is.na(site[[1]]@description))
    cu <- site[[1]]@collunits@collunits[[1]]
    testthat::expect_equal(cu@handle, "NEOPLACE")
    # Spelled `collectionunittype`, `unittype` and `collunittype` in turn.
    testthat::expect_equal(cu@collunittype, "Core")
  }
})

test_that("parse_site keeps the dataset fields the API reports", {
  # `datasetnotes`, `agerange$units` and `datasetpi` each used to be lost
  # between `parse_site()` and `build_dataset()`.
  result <- jsonlite::fromJSON(test_path("fixtures", "data_datasets24.json"),
                               simplifyVector = FALSE)
  sites <- neotoma2:::parse_site(result)
  ds <- sites[[1]]@collunits@collunits[[1]]@datasets@datasets[[1]]
  testthat::expect_equal(ds@age_units, "Calendar years BP")
  testthat::expect_equal(ds@age_range_old, 8100)
  testthat::expect_equal(ds@age_range_young, 2900)
  testthat::expect_equal(unlist(ds@pi_list), "Doe, Jane A.")
  testthat::expect_true(grepl("Taxon counts", ds@notes))
})

test_that("parse_site reads the geopolitical units from a download", {
  result <- jsonlite::fromJSON(test_path("fixtures", "data_downloads24_dup.json"),
                               simplifyVector = FALSE)
  sites <- neotoma2:::parse_site(result)
  testthat::expect_setequal(unlist(sites[[1]]@geopolitical),
                            c("Freedonia", "Northern Province"))
})

test_that("parse_site reads chronology metadata from a download", {
  skip_on_cran()
  dl <- get_downloads(24)
  cu <- dl[[1]]@collunits@collunits[[1]]
  chrons <- cu@chronologies@chronologies
  # The chronology metadata (agemodel, age bounds) should be populated, not NA.
  agemodels <- vapply(chrons, function(z) z@agemodel, character(1))
  testthat::expect_true(any(!is.na(agemodels)))
})

test_that("a site returns the same counts from sites and datasets", {
  skip_on_cran()
  # Site 666 has three collection units. Whichever endpoint we ask, we should
  # end up with the same site, the same units and the same datasets.
  from_sites <- getids(get_sites(666))
  from_datasets <- getids(get_datasets(get_sites(666)))
  testthat::expect_setequal(unique(from_sites$collunitid),
                            unique(from_datasets$collunitid))
  testthat::expect_setequal(unique(from_sites$datasetid),
                            unique(from_datasets$datasetid))
})
