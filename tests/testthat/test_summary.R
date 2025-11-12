library("testthat")
library("neotoma2")

test_that("`datasets()`", {
  skip_on_cran()
  dls <- get_sites() %>%
    get_downloads()
  sumDl <- summary(dls)
  ids <- getids(dls)
  dss <- datasets(dls)
  cus <- collunits(dls)
  testthat::expect_identical(sum(sumDl$n_datasets), nrow(ids))
  testthat::expect_identical(length(unique(sumDl$siteid)), length(dls))
  testthat::expect_true(all(sumDl$collunit_name %in% cus$handle))
  testthat::expect_true(all(cus$handle %in% sumDl$collunit_name))
})
