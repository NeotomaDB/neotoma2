library("testthat")
library("neotoma2")

context("`chronologies()` function displays all chronologies for a download object")
test_that("`get_downloads()` fills up chronologies' slots.", {
  skip_on_cran()
  dl <- get_downloads(4716)
  chron <- dl %>% chronologies() %>% as.data.frame()

  testthat::expect_is(dl[[1]]@collunits[[1]]@chronologies, "chronologies")
  testthat::expect_true(nrow(chron) > 1)
  testthat::expect_true(any(chron$chronologyid == 2195))
  # Only one isdefault value
  # Careful, if the DB is wrong, so will the API and this test will fail
  testthat::expect_equal(sum(chron$isdefault), 1)
  testthat::expect_is(chron, "data.frame")
})