# Get specimens by datasetid
test_that("Manual loading fails if interactive is false.", {
  skip_on_cran()
  get_specimens(datasetid=c(19832,41610))
})



# Get specimens by specimenID
test_that("Manual loading fails if interactive is false.", {
  skip_on_cran()
  get_specimens(c(7,8))
})


# Get specimens from sites object
test_that("Manual loading fails if interactive is false.", {
  skip_on_cran()
  my_sites <- get_sites(c(13296, 5663, 24))
  my_specimens <- get_specimens(my_sites)
})
