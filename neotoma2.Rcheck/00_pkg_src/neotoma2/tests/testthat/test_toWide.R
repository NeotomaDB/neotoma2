# load libraries
library("testthat")
library("neotoma2")

context("Run Neotoma `test_toWide` only when not on CRAN")

test_that("Running toWide on a samples dataframe.", {
  
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  singlechron <- samples(get_downloads(4716))
  multichron <- samples(get_downloads(21007))
  
  singlechront <- testthat::expect_true({suppressWarnings(toWide(singlechron,ecologicalgroups = c("AVES", "CARN", "PRIM", "RODE"), 
                                               elementtype = c("bone/tooth/shell", "bone/bill", "bone/tooth"), 
                                               unit = "present/absent", operation="presence")); TRUE})
  
  multichront <- testthat::expect_true({suppressWarnings(toWide(multichron,ecologicalgroups = c("RODE", "ARTI", "SORI"), 
                                                elementtype = c("bone/tooth"), 
                                                unit = "present/absent", operation="presence")); TRUE})
})
