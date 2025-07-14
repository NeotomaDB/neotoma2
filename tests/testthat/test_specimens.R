# Get specimens by datasetid
test_that("Manual loading fails if interactive is false.", {
  skip_on_cran()
  skip_if(TRUE, "Not ready yet")
  get_specimens(datasetid = c(19832, 41610))
})

# Get specimens by specimenID
test_that("Manual loading fails if interactive is false.", {
  skip_on_cran()
  skip_if(TRUE, "Not ready yet")
  get_specimens(c(7, 8))
})


# Get specimens from sites object
test_that("Manual loading fails if interactive is false.", {
  skip_on_cran()
  skip_if(TRUE, "Not ready yet")
  my_sites <- get_sites(c(13296, 5663, 24))
  my_specimens <- get_specimens(my_sites)
})

test_that("Building a specimen works.", {
  skip_if(TRUE, "Not ready yet")
  dataset <- data.frame(datasetid = 12,
    repo = c("Manhattan", "Banana"),
    taxonid = seq(1, 12))
  # Build error when we pass multiple rows.
  new_spec <- expect_error(build_specimen(dataset))
  new_spec <- expect_is(build_specimen(dataset[1, ]), "specimen")
})