library("testthat")
library("neotoma2")

context("`get_taxa()` and `get_taxon()` functions")
test_that("get_taxa() sites object", {
  skip_on_cran()
  # get_taxa(1) returns every site with this taxon, then downloads all of them:
  # a very large, flap-prone query. Skip on CI (GitHub Actions).
  skip_on_ci()
  abies <- get_taxa(1)
  testthat::expect_is(abies, "sites")
  abies_df <- abies %>% get_downloads() %>% taxa() %>% suppressWarnings()
  testthat::expect_true(any(grepl("Abies", abies_df$variablename)))
  abies <- get_taxa(taxonname = 'abies')
  testthat::expect_is(abies, "sites")
  abies_df <- abies %>% get_downloads() %>% taxa() %>% suppressWarnings()
  testthat::expect_true(any(grepl("Abies", abies_df$variablename)))
})