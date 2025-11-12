library("testthat")
library("neotoma2")

context("General tests")
test_that("Make sure get_downloads has chronologies.", {
  skip_on_cran()
  getchron <- get_downloads(4716) %>%
    chronologies() %>%
    as.data.frame()
  singlechron <- testthat::expect_true(any(getchron$chronologyid == 2195))
})

test_that("Get the samples out of dataset 15692.", {
  skip_on_cran()
  df <- get_downloads(15692) %>% samples()
  testthat::expect_gt(nrow(df), 1)
  testthat::expect_is(df, "data.frame")
})

test_that("ggplot2 on samples", {
  skip_on_cran()
  my_datasets <- get_datasets(40945)
  my_sites <- get_downloads(my_datasets)
  my_counts <- neotoma2::samples(my_sites)
  aa <- my_counts %>%
    dplyr::filter(taxongroup == "Vascular plants") %>%
    group_by(age, ecologicalgroup) %>%
    summarize(count = sum(value)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(x = age,
                                    y = count,
                                    color = ecologicalgroup))
  testthat::expect_is(aa, "gg")
})


datasetids <- c(41625, 46798, 48891, 47771,
                41620, 46689, 52833, 48756, 
                52742, 46700, 47608, 46841, 
                49238)
for (i in datasetids) {
  test_that(paste0("Duplicated sampleids for Dataset ID, ",
                   i, " don't exist (in the APD)"), {
    skip_on_cran()
    L <- get_datasets(i) %>%
      get_downloads()
    my_counts <- samples(L)
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
  })
}

test_that("A faunmap dataset", {
  mydataset <- get_downloads(7032)
  testthat::expect_is(mydataset, "sites")
})

test_that("taxa() returns only unique results", {
  skip_on_cran()
  mydataset <- get_downloads(c(1435, 24238))
  testthat::expect_false(any(duplicated(taxa(mydataset))))
})

test_that("If B is contained in A region,
          get_sites() from B will be contained
          in get_sites() from A", {
            skip_on_cran()
            location <- '{"type": "Polygon",
            "coordinates": [[
                [-169, 24],
                [-169, 75],
                [-52, 75],
                [-52, 24],
                [-169, 24]]]}'
            usa <- get_sites(loc = location, limit = 20000)
            fla <- get_sites(gpid = "Florida", limit = 10000)
            testthat::expect_true(all(getids(fla)$siteid %in% getids(usa)$siteid))
          })

test_that("Publications calls.", {
  empty <- get_publications()
  pollen <- get_publications(search = "pollen")
  counts <- get_publications(c(1, 2, 3, 4))
  frompubs <- get_publications(counts)
  testthat::expect_is(empty, "publications")
  testthat::expect_is(pollen, "publications")
  testthat::expect_is(counts, "publications")
  testthat::expect_false(empty[[1]]@citation == pollen[[1]]@citation)
  testthat::expect_length(counts, 4)
  testthat::expect_identical(counts, frompubs)
})