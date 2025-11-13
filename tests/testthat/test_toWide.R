library("testthat")
library("neotoma2")

context("Apply a similar version to `tidyr::toWide` for
        `neotoma2` samples dataframes.")
test_that("`toWide()` on a samples dataframe.", {
  skip_on_cran()
  fauna1 <- samples(get_downloads(4716))
  fauna1t <- toWide(fauna1,
                 ecologicalgroups = c("AVES", "CARN", "PRIM", "RODE"), 
                 elementtype = c("bone/tooth/shell", "bone/bill", "bone/tooth"), 
                 unit = "present/absent", 
                 operation="presence")
  testthat::expect_true(is.data.frame(fauna1t), single_w)
  fauna2 <- samples(get_downloads(21007))
  fauna2t <- toWide(fauna2,
                        ecologicalgroups = c("RODE", "ARTI", "SORI"), 
                        elementtype = c("bone/tooth"), 
                        unit = "present/absent", operation="presence")
  testthat::expect_true(is.data.frame(fauna2t), TRUE)
})

test_that("`toWide()` proportions calculation.", {
  skip_on_cran()
  alex <- samples(get_downloads(24))
  alext <- toWide(alex,
                  unit = "NISP",
                  operation = "prop")
  nuph4129 <- alex %>%
    dplyr::filter(variablename == "Nuphar", age == 4129)
  # calculate the proportion manually from alex
  total_nisp_4129 <- sum(alex$value[alex$age == 4129 & alex$unit == "NISP"], na.rm = TRUE)
  nuphprop_4129 <- sum(nuph4129$value, na.rm = TRUE) / total_nisp_4129
  # extract the proportion from the wide data frame
  nuphprop_wide_4129 <- alext$Nuphar[alext$age == 4129]
  testthat::expect_equal(nuphprop_4129, nuphprop_wide_4129)
})

test_that("`toWide()` sum operation works", {
  skip_on_cran()
  alex <- samples(get_downloads(24))
  alext <- toWide(alex,
                  unit = "NISP",
                  operation = "sum")
  nuph4129 <- alex %>%
    dplyr::filter(variablename == "Nuphar", age == 4129)
  # calculate the sum manually from alex
  nuph_sum_4129 <- sum(nuph4129$value, na.rm = TRUE)
  # extract the sum from the wide data frame
  nuph_sum_wide_4129 <- alext$Nuphar[alext$age == 4129]
  testthat::expect_equal(nuph_sum_4129, nuph_sum_wide_4129)
})