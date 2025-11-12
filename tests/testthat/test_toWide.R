library("testthat")
library("neotoma2")

context("Apply a similar version to `tidyr::toWide` for
        `neotoma2` samples dataframes.")
test_that("`toWide()` on a samples dataframe.", {
  skip_on_cran()
  singlechron <- samples(get_downloads(4716))
  multichron <- samples(get_downloads(21007))
  singlechron <- toWide(singlechron,
                 ecologicalgroups = c("AVES", "CARN", "PRIM", "RODE"), 
                 elementtype = c("bone/tooth/shell", "bone/bill", "bone/tooth"), 
                 unit = "present/absent", 
                 operation="presence")
  testthat::expect_true(is.data.frame(singlechron), single_w)
  multichront <- toWide(multichron,
                        ecologicalgroups = c("RODE", "ARTI", "SORI"), 
                        elementtype = c("bone/tooth"), 
                        unit = "present/absent", operation="presence")
  testthat::expect_true(is.data.frame(multichront), TRUE)
})
