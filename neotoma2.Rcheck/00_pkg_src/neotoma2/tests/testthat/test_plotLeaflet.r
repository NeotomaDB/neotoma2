# load libraries
testthat::test_that("We can download records and plot them 
  with plot leaflet.", {

  ## we don't want this to run on CRAN
  skip_on_cran()
  fiftyds <- get_datasets(limit = 50)

  output <- plotLeaflet(fiftyds)
  testthat::expect_is(output, "leaflet")

})

testthat::test_that("We can download records and plot a single site
  with plot leaflet.", {

  ## we don't want this to run on CRAN
  skip_on_cran()
  fiftyds <- get_datasets(limit = 50)

  output <- plotLeaflet(fiftyds[[1]])
  testthat::expect_is(output, "leaflet")
})
