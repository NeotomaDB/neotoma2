library("testthat")
library("neotoma2")

context("Creating publications from scratch or updating existing objects.")
test_that("`set_publication()` for a new publication", {
  skip_on_cran()
  new_pub <- set_publication(
    publicationid = 56,
    citation = "Citation for my publication",
    doi = "fake.doi/10.1234/fake"
  )
  testthat::expect_is(new_pub, "publication")
  testthat::expect_equal(new_pub@publicationid, 56)
  testthat::expect_equal(new_pub@citation, "Citation for my publication")
  testthat::expect_equal(new_pub@doi, "fake.doi/10.1234/fake")
})

test_that("Updating existing publication", {
  skip_on_cran()
  # Cannot update over a sites object:
  download_pub <- get_publications(search = "pollen", limit = 1)
  fake_doi <- "fake.doi/10.1234/fake"
  testthat::expect_error(set_publications(x = download_pub, doi = fake_doi))
  testthat::expect_is(set_publication(x = download_pub[[1]], doi = fake_doi),
                      "publication")
  updated_pub <- set_publication(x = download_pub[[1]], doi = fake_doi)
  testthat::expect_equal(updated_pub@doi, "fake.doi/10.1234/fake")
})
