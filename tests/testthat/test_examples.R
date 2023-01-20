test_that("Examples run without error", {
  ## we don't want this to run on CRAN
  skip_on_cran()

  ## List of example topics we want to check
  egs <- c("get_sites",
           "get_datasets",
           "get_downloads",
           "filter",
           "taxa",
           "testNULL",
           "samples",
           "get_publications",
           "clean")

  for (i in seq_along(egs)) {
    egout <- try(example(topic = egs[i], package = "neotoma2", ask = FALSE,
                         character.only = TRUE, run.dontrun = TRUE,
                         echo = TRUE))

  }
})