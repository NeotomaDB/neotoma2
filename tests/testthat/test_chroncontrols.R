test_that("Obtaining chron controls for differrent kinds of records works as expected.", {

  ## we don't want this to run on CRAN
  skip_on_cran()
  single <- chroncontrols(get_downloads(4716))
  multi <- chroncontrols(get_downloads(21007))
  mamchron <- chroncontrols(get_downloads(4564))

  testthat::expect_is(single, "data.frame")
  testthat::expect_equal(length(unique(single$chronologyid)), 3)
  testthat::expect_is(multi, "data.frame")
  testthat::expect_gt(length(unique(multi$chronologyid)), 4)
  testthat::expect_is(mamchron, "data.frame")

})

test_that("We can set and change the default chronology.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  site <- get_downloads(24238)
  chrono <- chronologies(site)
  newchron <- set_default(chrono, 14590)
  df_default <- as.data.frame(chrono)$isdefault
  df_newchron <- as.data.frame(newchron)$isdefault

  tester <- all(df_default == df_newchron)
  testthat::expect_false(tester)
})

test_that("We can add a new chronology to a record:",
{

  skip_on_cran()
  stara <- get_downloads(24238)
  stara_chron <- chronologies(stara)

  controls <- chroncontrols(stara) %>%
    dplyr::filter(chronologyid == 14591) %>%
    arrange(depth)

  controls$chroncontrolage[1] <- 0
  controls$agelimityounger[1] <- -2
  controls$agelimitolder[1] <- 2
  controls$thickness[1] <- 1

  predict_depths <- samples(stara) %>%
    select(depth, analysisunitid) %>%
    unique() %>%
    arrange(depth)

  new_chron <- Bchron::Bchronology(ages = controls$chroncontrolage,
                                  ageSds = abs(controls$agelimityounger -
                                                  controls$chroncontrolage),
                                  calCurves = c("normal", rep("intcal20", 4)),
                                  positionThicknesses = controls$thickness,
                                  positions = controls$depth,
                                  allowOutside = TRUE,
                                  ids = controls$chroncontrolid)

  newpredictions <- predict(new_chron, predict_depths$depth)

  new_chron_stara <- set_chronology(agemodel = "Bchron model",
                              isdefault = TRUE,
                              ageboundolder = max(newpredictions),
                              ageboundyounger = min(newpredictions),
                              dateprepared = lubridate::today(),
                              modelagetype = "Calibrated radiocarbon years BP",
                              chronologyname = "Simon's example chronology",
                              chroncontrols = controls)

  new_sample_ages <- data.frame(predict_depths,
                              age = colMeans(newpredictions),
                              ageolder = colMeans(newpredictions) +
                                apply(newpredictions, 2, sd),
                              ageyounger = colMeans(newpredictions) -
                                apply(newpredictions, 2, sd),
                              agetype = "Calibrated radiocarbon years")

  stara[[1]]$collunits[[1]] <- add_chronology(stara[[1]]$collunits[[1]],
      new_chron_stara,
      new_sample_ages)

  last_age <- samples(stara) %>%
    dplyr::filter(analysisunitid == 194633) %>%
    select(age) %>%
    unique()

  testthat::expect_true(last_age == new_sample_ages$age[nrow(new_sample_ages)])
})
