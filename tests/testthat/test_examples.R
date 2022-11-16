context("Run Neotoma examples only when not on CRAN")

test_that("Examples run without error", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  
  ## List of example topics we want to check
  egs <- c('get_sites',
           'get_datasets',
           #'get_publications', #write more examples
           'get_downloads')  
  
  refnames <- paste0("example-ref-", egs, ".rds")
  
  for (i in seq_along(egs)) {
    # Testing each of the example codes, by `topic`.  
    egout <- try(example(topic = egs[i], package = "neotoma2", ask = FALSE,
                         character.only = TRUE, run.dontrun = TRUE,
                         echo = TRUE))

  }
})