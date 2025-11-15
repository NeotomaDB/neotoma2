testthat::skip("Skipping all tests in this file")
library("testthat")
library("neotoma2")
library("tools")

context("Test that all examples run without error")
csv_path <- testthat::test_path("all_aliases.csv")
topics <- read.csv(csv_path, stringsAsFactors = FALSE)$alias
for (topic in topics) {
  topic <- sub(",.*$", "", topic)
  local({
    t <- topic
    test_that(paste0("Example for '", topic, "' runs without error"), {
      skip_on_cran()
      testthat::expect_error(
        example(topic = t,
                package = "neotoma2",
                ask = FALSE,
                character.only = TRUE,
                run.dontrun = TRUE,
                echo = FALSE),
        NA)
    })
  })
}