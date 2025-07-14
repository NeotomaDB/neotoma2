test_that("Conversion to JSON works as expected:", {
  skip_on_cran()
  skip_if(TRUE, "Not ready yet")
  # sites <- get_sites(sample(6000, 5))
  # sites_dl <- get_downloads(sites)
  # aa <- toJSON(sites)
  # aadl <- toJSON(sites_dl)
  # 
  # expect_is(aa, "json")
  # expect_is(aadl, "json")
  # expect_false(rlang::hash(aa) == rlang::hash(aadl))
  # 
  # render <- jsonlite::fromJSON(aadl,
  #                                  flatten = FALSE,
  #                                  simplifyVector = FALSE)
  # expect_is(render, "list")
})
