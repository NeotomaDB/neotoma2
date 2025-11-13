library("testthat")
library("neotoma2")

context("`get_contacts()` tests")
test_that("`get_contacts()` numeric", {
  single <- get_contacts(x = 1)
  testthat::expect_equal(length(single), 1)
  testthat::expect_true(class(single) == "contacts")
  testthat::expect_true(class(single[[1]]) == "contact")
  
  multiple <- get_contacts(x = c(1, 2, 3))
  testthat::expect_equal(length(multiple), 3)
  testthat::expect_true(class(multiple) == "contacts")
  testthat::expect_true(all(sapply(multiple@contacts, function(x) {
    class(x) == "contact"
  })))
})

test_that("`get_contacts()` query", {
  goring <- get_contacts(familyname ="Goring")
  testthat::expect_gt(length(goring), 0)
  testthat::expect_true(class(goring) == "contacts")
  testthat::expect_true(class(goring[[1]]) == "contact")
})