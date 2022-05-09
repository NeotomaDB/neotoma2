# load libraries
library("testthat")
library("neotoma2")
library("tidyverse")

context("Run only when not on CRAN")

test_that("Some datasets don't seem to get pulled with chronologies.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  getchron <- get_downloads(4716) %>%
    chronologies() %>%
    as.data.frame()

  singlechron <- testthat::expect_true(any(getchron$chronologyid == 2195))

})

test_that("We can get the samples out of dataset 15692.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  testthat::expect_is(get_downloads(15692) %>% samples(), "data.frame")
})

test_that("ggplot2 on the african data works:", {
  my_datasets <- get_datasets(40945)
  mySites <- get_downloads(my_datasets)
  my_counts <- neotoma2::samples(mySites)

  aa <- my_counts %>%
    filter(taxongroup == "Vascular plants") %>%
    group_by(age, ecologicalgroup) %>%
    summarize(count = sum(value)) %>%
    ggplot() +
    geom_path(aes(x = age, y = count, color = ecologicalgroup))
  testthat::expect_is(aa, "gg")
})
