context("Run general tests only when not on CRAN")

test_that("Some datasets don't seem to get pulled with chronologies.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  getchron <- get_downloads(4716) %>%
    chronologies() %>%
    as.data.frame()

  singlechron <- testthat::expect_true(any(getchron$chronologyid == 2195))

})

test_that("We can get the samples out of dataset 15692.", {
  ## we don't want this to run on CRAN
  skip_on_cran()
  testthat::expect_is(get_downloads(15692) %>% samples(), "data.frame")
})

test_that("ggplot2 on the african data works:", {
  my_datasets <- get_datasets(40945)
  mySites <- get_downloads(my_datasets)
  my_counts <- neotoma2::samples(mySites)

  aa <- my_counts %>%
    dplyr::filter(taxongroup == "Vascular plants") %>%
    group_by(age, ecologicalgroup) %>%
    summarize(count = sum(value)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(x = age, y = count, color = ecologicalgroup))
  testthat::expect_is(aa, "gg")
})

test_that("Duplicated sampleids don't exist (in the APD)", {

  datasetids <- c(41625, 46798, 48891, 47771, 41620, 46689, 52833,
                  48756, 52742, 46700, 47608, 46841, 49238)

  for (i in 1:length(datasetids)) {
    L <- neotoma2::get_datasets(41625) %>%
      neotoma2::get_downloads()
    my_counts <- neotoma2::samples(L)
    assertthat::assert_that(!any(duplicated(my_counts)))

    counts <- pivot_wider(my_counts,
                          names_from = variablename,
                          values_from = value,
                          values_fill = NA,
                          id_cols = age) %>%
      tryCatch(.data,
               error = function(e) e,
               warning = function(w) w)

    testthat::expect_false(any(duplicated(my_counts)))
    testthat::expect_false(is(counts, "warning"))
  }
})

test_that("A faunmap dataset with some contacts actually works", {
  mydataset <- get_downloads(7032)
  testthat::expect_is(mydataset, "sites")
})

test_that("The taxa() call should only return unique results", {
  mydataset <- get_downloads(c(1435, 24238))
  testthat::expect_false(any(duplicated(taxa(mydataset))))
})

testthat::test_that("Location parsing isn't affecting the representation of spatial polygons passed to the DB:", {
  # This is an issue raised by Adrian.

  location <- '{"type": "Polygon",
            "coordinates": [[
                [-169, 24],
                [-169, 75],
                [-52, 75],
                [-52, 24],
                [-169, 24]]]}'

  loc1 <- geojsonsf::geojson_sf(location) # sf object
  loc <- geojsonsf::sf_geojson(loc1)

  testthat::expect_equivalent(loc1, geojsonsf::geojson_sf(loc))
  testthat::expect_equivalent(loc1,
    geojsonsf::geojson_sf(parse_location(location)))
})

testthat::test_that("We are pulling in the sites we expect to capture:", {
  # This test takes a really long time. . .
  skip_on_cran()
  location <- '{"type": "Polygon",
            "coordinates": [[
                [-169, 24],
                [-169, 75],
                [-52, 75],
                [-52, 24],
                [-169, 24]]]}'

  usa <- get_sites(loc = location, limit = 16340)
  #usa_ds <- get_datasets(loc = location, all_data = TRUE)
  fla <- get_sites(gpid = "Florida", limit = 10000)

  testthat::expect_true(all(getids(fla)$siteid %in% getids(usa)$siteid))
  #testthat::expect_true(all(getids(fla)$siteid %in% getids(usa_ds)$siteid))

})
