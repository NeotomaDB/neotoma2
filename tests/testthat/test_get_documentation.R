library("testthat")
library("neotoma2")

context("Retrieves Neotoma2 R package documentation in HTML")
test_that("Manual loading fails if interactive is false.", {
  skip_on_cran()
  testthat::expect_error(get_documentation())
})

test_that("Documentation loading succeeds when interactive is true:", {
  skip_on_cran()
  # Stub the browser so the headless CI runner does not shell out to xdg-open
  # (which prints "no method available" noise into the log). Capturing the URL
  # also lets us assert which page the function opens.
  seen <- NULL
  op <- options(browser = function(url) {
    seen <<- url
    invisible(NULL)
  })
  on.exit(options(op), add = TRUE)
  testthat::expect_null(rlang::with_interactive(get_documentation(),
                                                value = TRUE))
  testthat::expect_equal(seen, "https://open.neotomadb.org/neotoma2/")
})