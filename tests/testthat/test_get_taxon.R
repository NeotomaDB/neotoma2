library("testthat")
library("neotoma2")

test_that("`get_taxon` yields specific taxon information", {
  skip_on_cran()
  abies_num <- get_taxon(1)
  testthat::expect_is(abies_num, "taxa")
  testthat::expect_is(abies_num[[1]], "taxon")
  
  abies_name <- get_taxon(taxonname = "abies")
  testthat::expect_is(abies_name, "taxa")
  testthat::expect_is(abies_name[[1]], "taxon")
  testthat::expect_equal(as.data.frame(abies_num), as.data.frame(abies_name))
})