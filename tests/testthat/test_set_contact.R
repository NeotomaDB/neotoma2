library("testthat")
library("neotoma2")

context("Contact object is set up properly.")
test_that("check_contacts works properly.", {
  skip_on_cran()
  testthat::expect_error(check_contacts(12))
  testthat::expect_error(check_contacts("party"))
  new_name <- c(set_contact(familyname = "Goring"),
    set_contact(familyname = "Cullen"))
  testthat::expect_is(check_contacts(new_name), "list")
  na_name <- c(set_contact(familyname = "Goring", contactid = NA),
    set_contact(familyname = "Cullen"))
  testthat::expect_is(check_contacts(na_name), "list")
})