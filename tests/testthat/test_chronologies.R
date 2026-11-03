library("testthat")
library("neotoma2")
library("httptest")

context("`chronologies()` displays all chronologies for a download object")
httptest::with_mock_api({
  test_that("`get_downloads()` fills up chronologies' slots.", {
    skip_on_cran()
    dl <- get_downloads(4716)
    chron <- dl %>%
      chronologies() %>%
      as.data.frame()

    testthat::expect_is(dl[[1]]@collunits[[1]]@chronologies, "chronologies")
    testthat::expect_true(nrow(chron) > 1)
    testthat::expect_true(any(chron$chronologyid == 2195))
    # Only one isdefault value
    # Careful, if the DB is wrong, so will the API and this test will fail
    testthat::expect_equal(sum(chron$isdefault), 1)
    testthat::expect_is(chron, "data.frame")
  })
})

context("`chronologies()` function is empty for empty downloads")
httptest::with_mock_api({
  test_that("`chronologies()` is empty for a record with no chronology", {
    skip_on_cran()
    dl <- get_downloads(100)
    chron <- dl %>% chronologies()

    testthat::expect_equal(length(chron@chronologies), 0)
  })
})

context("`chronologies()` ties each chronology to its site and dataset")
httptest::with_mock_api({
  test_that("as.data.frame(chronologies()) has siteid, collunitid, datasetid", {
    skip_on_cran()
    both <- c(get_downloads(4716), get_downloads(21007))
    chron <- as.data.frame(chronologies(both))
    ids <- getids(both)

    idcols <- c("siteid", "collunitid", "datasetid", "chronologyid")
    testthat::expect_true(all(idcols %in% names(chron)))
    testthat::expect_false(anyNA(chron[c("siteid", "collunitid", "datasetid")]))
    # Every (site, collunit, dataset) triple is a real one from the download.
    triples <- unique(chron[c("siteid", "collunitid", "datasetid")])
    testthat::expect_equal(nrow(dplyr::inner_join(
      triples, ids,
      by = c("siteid", "collunitid", "datasetid")
    )), nrow(triples))
    # The two sites keep their own chronologies apart.
    ds_of <- function(id) unique(chron$datasetid[chron$chronologyid == id])
    testthat::expect_equal(ds_of(2195), 4716)
    testthat::expect_equal(ds_of(12111), 21007)
    # c() on chronologies keeps the ids.
    testthat::expect_equal(
      sort(unique(chron$datasetid)), c(4716, 21007)
    )
  })

  test_that("a collection unit with several datasets repeats each chronology", {
    skip_on_cran()
    cu <- get_downloads(4716)[[1]]@collunits[[1]]
    ds1 <- cu@datasets@datasets[[1]]
    ds2 <- ds1
    ds2@datasetid <- 99999L
    cu@datasets@datasets <- list(ds1, ds2)
    chron <- as.data.frame(chronologies(cu))

    testthat::expect_equal(nrow(chron), 2 * length(chronologies(cu)))
    testthat::expect_setequal(chron$datasetid, c(4716, 99999))
    testthat::expect_equal(
      as.vector(table(chron$chronologyid)), rep(2, length(chronologies(cu)))
    )
    # The collection unit is known; the site is not, from a collunit alone.
    testthat::expect_true(all(chron$collunitid == 4571))
    testthat::expect_true(all(is.na(chron$siteid)))
  })

  test_that("set_default still works on the longer table", {
    skip_on_cran()
    chrons <- chronologies(get_downloads(4716))
    newdef <- set_default(chrons, 47129)
    out <- as.data.frame(newdef)
    testthat::expect_equal(nrow(out), nrow(as.data.frame(chrons)))
    testthat::expect_true(out$isdefault[out$chronologyid == "47129"])
    # 47130 shares its model age type with 47129, so it is no longer default.
    testthat::expect_false(out$isdefault[out$chronologyid == "47130"])
  })
})

test_that("a hand-built chronology has NA ids", {
  chron <- as.data.frame(new("chronology", chronologyid = 1L))
  testthat::expect_true(all(
    is.na(chron[c("siteid", "collunitid", "datasetid")])
  ))
  testthat::expect_equal(nrow(chron), 1)
})
