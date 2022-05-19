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
    dplyr::filter(taxongroup == "Vascular plants") %>%
    group_by(age, ecologicalgroup) %>%
    summarize(count = sum(value)) %>%
    ggplot() +
    geom_path(aes(x = age, y = count, color = ecologicalgroup))
  testthat::expect_is(aa, "gg")
})

test_that("Duplicated sampleids don't exist (in the APD)", {

  datasetids <- c(41625, 46798, 48891, 47771, 41620, 46689, 52833,
                  48756, 52742, 46700, 47608, 46841, 49238)

  for (i in 1:length(datasetids)) {
    L <- neotoma2::get_datasets(41625) %>%
      neotoma2::get_downloads()
    my_counts <- neotoma2::samples(L)
    assertthat::assert_that(!any(duplicated(my_counts)))

    counts <- pivot_wider(my_counts,
                          names_from = variablename,
                          values_from = value,
                          values_fill = NA,
                          id_cols = age) %>%
      tryCatch(.data,
               error = function(e) e,
               warning = function(w) w)

    testthat::expect_false(any(duplicated(my_counts)))
    testthat::expect_false(is(counts, "warning"))
  }
})
