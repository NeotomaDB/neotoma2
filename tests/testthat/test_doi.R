library("testthat")
library("neotoma2")

context("`doi()` function")
test_that("", {
  sts <- get_datasets(c(24, 100, 101))
  dois <- doi(sts)
  testthat::expect_is(dois, "data.frame")
  st24 <- sts[[1]]
  doi24 <- purrr::map(datasets(st24)@datasets,
                      function(x)  {
                        doi <- x@doi
                        unlist(doi, recursive = TRUE, use.names = FALSE)
                        }) %>% unlist()
  dois24_df <- dois %>% dplyr::filter(siteid == 24) %>% dplyr::pull(doi) %>% unlist()
  testthat::expect_true(all(doi24 %in% dois24_df))
  testthat::expect_true(all(dois24_df %in% doi24))

  st100 <- sts[[2]]
  doi100 <- purrr::map(datasets(st100)@datasets,
                       function(x)  {
                         doi <- x@doi
                         unlist(doi, recursive = TRUE, use.names = FALSE)
                       }) %>% unlist()
  dois100_df <- dois %>% dplyr::filter(siteid == 100) %>% dplyr::pull(doi) %>% unlist()
  testthat::expect_true(all(doi100 %in% dois100_df))
  testthat::expect_true(all(dois100_df %in% doi100))
  
  st101 <- sts[[3]]
  doi101 <- purrr::map(datasets(st101)@datasets,
                          function(x)  {
                            doi <- x@doi
                            unlist(doi, recursive = TRUE, use.names = FALSE)
                          }) %>% unlist()
  dois101_df <- dois %>% dplyr::filter(siteid == 101) %>% dplyr::pull(doi) %>% unlist()
  testthat::expect_true(all(doi101 %in% dois101_df))
  testthat::expect_true(all(dois101_df %in% doi101))
})