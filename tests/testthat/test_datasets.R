library("testthat")
library("neotoma2")

test_that("`datasets()`", {
  skip_on_cran()
  dls <- get_sites() %>%
    get_downloads()
  sumDl <- summary(dls)
  ids <- getids(dls)
  dss <- datasets(dls)
  testthat::expect_identical(length(dss), nrow(ids))
  testthat::expect_identical(sum(sumDl$n_datasets), nrow(ids))
  testthat::expect_identical(length(unique(dss$datasetid)),
                             length(unique(ids$datasetid)))
})
