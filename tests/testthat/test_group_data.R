library("testthat")
library("neotoma2")

context("Test `group_response()` folds every endpoint into one shape.")

test_that("pick takes the first spelling the API used", {
  # The collection unit type arrives under a different name from each endpoint.
  cu_site <- list(collectionunittype = "Core")
  cu_dataset <- list(unittype = "Section")
  cu_download <- list(collunittype = "Core")
  spellings <- c("collectionunittype", "unittype", "collunittype")
  testthat::expect_equal(neotoma2:::pick(cu_site, spellings), "Core")
  testthat::expect_equal(neotoma2:::pick(cu_dataset, spellings), "Section")
  testthat::expect_equal(neotoma2:::pick(cu_download, spellings), "Core")
  # A field none of the spellings match is NULL, not an error.
  testthat::expect_null(neotoma2:::pick(list(handle = "ABC"), spellings))
  # A NULL value is skipped in favour of the next spelling.
  testthat::expect_equal(neotoma2:::pick(list(unittype = NULL,
                                              collunittype = "Core"),
                                         spellings), "Core")
})

test_that("pick does not partially match a field name", {
  # `$` would match `collunittype` from `collunit`; `pick()` must not.
  testthat::expect_null(neotoma2:::pick(list(collunittype = "Core"),
                                        "collunit"))
})

test_that("group_response gives every endpoint the same shape", {
  files <- c("data_sites24.json", "data_datasets24.json",
             "data_downloads24_dup.json")
  for (f in files) {
    result <- jsonlite::fromJSON(test_path("fixtures", f),
                                 simplifyVector = FALSE)
    grouped <- neotoma2:::group_response(result$data)
    # One site, reached the same way regardless of which endpoint answered.
    testthat::expect_length(grouped, 1)
    testthat::expect_equal(grouped[[1]]$site$siteid, 24)
    testthat::expect_length(grouped[[1]]$site$collectionunits, 1)
    cu <- grouped[[1]]$site$collectionunits[[1]]
    testthat::expect_equal(cu$collectionunitid, 24)
  }
})

test_that("group_response returns an empty list for an empty response", {
  testthat::expect_length(neotoma2:::group_response(list()), 0)
})

test_that("group_collunits folds repeated units and pools their datasets", {
  cus <- list(
    list(collectionunitid = 24, handle = "ALEXLAKE",
         datasets = list(list(datasetid = 24))),
    list(collectionunitid = 24, handle = "ALEXLAKE",
         datasets = list(list(datasetid = 7870)))
  )
  folded <- neotoma2:::group_collunits(cus)
  testthat::expect_length(folded, 1)
  testthat::expect_equal(folded[[1]]$collectionunitid, 24)
  testthat::expect_setequal(sapply(folded[[1]]$datasets, function(x) {
    x$datasetid
  }), c(24, 7870))
})

test_that("group_collunits keeps genuinely different units apart", {
  cus <- list(
    list(collectionunitid = 1, datasets = list(list(datasetid = 10))),
    list(collectionunitid = 2, datasets = list(list(datasetid = 20)))
  )
  testthat::expect_length(neotoma2:::group_collunits(cus), 2)
})

test_that("unique_datasets reports each dataset once", {
  datasets <- list(list(datasetid = 24), list(datasetid = 24),
                   list(datasetid = 7870))
  kept <- neotoma2:::unique_datasets(datasets)
  testthat::expect_length(kept, 2)
  testthat::expect_setequal(sapply(kept, function(x) x$datasetid),
                            c(24, 7870))
})

test_that("group_sites keeps the order the API sent", {
  # `split()` on its own would sort these to 1001, 24, 3.
  elements <- lapply(c(24, 1001, 3), function(id) {
    list(site = list(siteid = id), collectionunits = list())
  })
  grouped <- neotoma2:::group_sites(elements)
  testthat::expect_equal(sapply(grouped, function(x) x$site$siteid),
                         c(24, 1001, 3))
})

test_that("group_sites folds elements that share a site", {
  # `downloads` reports one element per dataset, so a site can arrive twice.
  elements <- list(
    list(site = list(siteid = 24),
         collectionunits = list(list(collectionunitid = 24,
                                     datasets = list(list(datasetid = 24))))),
    list(site = list(siteid = 24),
         collectionunits = list(list(collectionunitid = 24,
                                     datasets = list(list(datasetid = 7870)))))
  )
  grouped <- neotoma2:::group_sites(elements)
  testthat::expect_length(grouped, 1)
  testthat::expect_length(grouped[[1]]$site$collectionunits, 1)
  testthat::expect_length(grouped[[1]]$site$collectionunits[[1]]$datasets, 2)
})
