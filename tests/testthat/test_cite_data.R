library("testthat")
library("neotoma2")
library("dplyr")
library("stringr")

context("`cite_data()` function")
test_that("cite_data() returns dataframe", {
  skip_on_cran()
  st <- get_sites(c(24, 100))
  citation <- cite_data(st)
  testthat::expect_is(citation, "data.frame")
})

test_that("cite_data() returns correct DOIs for each dataset", {
  skip_on_cran()
  st <- get_datasets(c(24, 100))
  citation <- cite_data(st)
  cit24 <- citation %>% 
    dplyr::filter(datasetid == 24)
  cit24 <- cit24$citation
  doi24 <- st[[1]]@collunits[[1]]@datasets[[1]]@doi %>% unlist()
  # expect that doi is in citation string
  testthat::expect_true(any(str_detect(cit24, fixed(doi24))))
  cit100 <- citation %>% 
    filter(datasetid == 100)
  cit100 <- cit100$citation
  doi100 <- st[[2]]@collunits[[1]]@datasets[[1]]@doi %>% unlist()
  # expect that doi is in citation string
  testthat::expect_true(any(str_detect(cit100, fixed(doi100))))
  testthat::expect_false(any(str_detect(cit100, fixed(doi24))))
  testthat::expect_false(any(str_detect(cit24, fixed(doi100))))
})