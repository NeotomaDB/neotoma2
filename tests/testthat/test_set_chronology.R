library("testthat")
library("neotoma2")
library("dplyr")

test_that("Add a new chronology to a record:", {
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
                                    dateprepared = Sys.Date(),
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