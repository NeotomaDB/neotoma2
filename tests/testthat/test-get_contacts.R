test_that("Non integer x returns nothing:", {
  noint <- get_contacts(x = "Goring")
  expect_length(noint, 1)
})

test_that("Integer x returns sets of contacts:", {
  single <- get_contacts(x = 1)
  expect_true(class(single) == "contacts")
})