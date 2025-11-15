library("testthat")
library("neotoma2")

test_that("collunits()`", {
  skip_on_cran()
  dls <- get_sites() %>%
    get_downloads()
  sumDl <- summary(dls)
  ids <- getids(dls)
  cus <- collunits(dls)
  testthat::expect_identical(length(unique(cus$collectionunitid)),
                             length(unique(ids$collunitid)))
  testthat::expect_true(all(sumDl$collunit_name %in% cus$handle))
})