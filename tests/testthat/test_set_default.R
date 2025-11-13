library("testthat")
library("neotoma2")

test_that("Set and change the default chronology.", {
  skip_on_cran()
  site <- get_downloads(24238)
  chrono <- chronologies(site)
  newchron <- set_default(chrono, 14590)
  df_default <- as.data.frame(chrono)$isdefault
  df_newchron <- as.data.frame(newchron)$isdefault
  tester <- all(df_default == df_newchron)
  testthat::expect_false(tester)
})