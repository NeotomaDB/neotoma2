library("testthat")
library("neotoma2")

context("Creating publications from scratch or updating existing objects.")
test_that("`set_publication()` for a new publication", {
  skip_on_cran()
  newPub <- set_publication(publicationid = 56,
                            citation = "Citation for my publication",
                            doi = "fake.doi/10.1234/fake")
  testthat::expect_is(newPub, "publication")
  testthat::expect_equal(newPub@publicationid, 56)
  testthat::expect_equal(newPub@citation, "Citation for my publication")
  testthat::expect_equal(newPub@doi, "fake.doi/10.1234/fake")
})

test_that("Updating existing publication", {
  skip_on_cran()
  # Cannot update over a sites object:
  downloadPub <- get_publications(search = "pollen", limit = 1)
  testthat::expect_error(set_publications(x = downloadPub, doi = "fake.doi/10.1234/fake"))
  testthat::expect_is(set_publication(x = downloadPub[[1]], doi = "fake.doi/10.1234/fake"), "publication")
  updatedPub <- set_publication(x = downloadPub[[1]], doi = "fake.doi/10.1234/fake")
  testthat::expect_equal(updatedPub@doi, "fake.doi/10.1234/fake")
})