# load libraries
library("testthat")
library("neotoma2")
library("Bchron")

context("Test the runs for chroncontrols.")

test_that("Running samples on a record with multiple chronologies pulls the default model.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  singlechron <- testthat::expect_true({chroncontrols(get_downloads(4716)); TRUE})
  multichron <- testthat::expect_true({chroncontrols(get_downloads(21007)); TRUE})
  mammals <- testthat::expect_true({chroncontrols(get_downloads(4564)); TRUE})

})

test_that("We can set and change the default chronology.", {

  ## we don't want this to run on CRAN

  skip_on_cran()
  site <- get_downloads(24238)
  chrono <- chronologies(site)
  newchron <- set_default(chrono, 14590)

  tester <- all(as.data.frame(chrono)$isdefault == as.data.frame(newchron)$isdefault)
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

            predictDepths <- samples(stara) %>%
              select(depth, analysisunitid) %>%
              unique() %>%
              arrange(depth)

            newChron <- Bchron::Bchronology(ages = controls$chroncontrolage,
                                            ageSds = abs(controls$agelimityounger -
                                                           controls$chroncontrolage),
                                            calCurves = c("normal", rep("intcal20", 4)),
                                            positionThicknesses = controls$thickness,
                                            positions = controls$depth,
                                            allowOutside = TRUE,
                                            ids = controls$chroncontrolid)

            newpredictions <- predict(newChron, predictDepths$depth)

            newChronStara <- set_chronology(agemodel = "Bchron model",
                                       isdefault = 1,
                                       ageboundolder = max(newpredictions),
                                       ageboundyounger = min(newpredictions),
                                       dateprepared = lubridate::today(),
                                       modelagetype = "Calibrated radiocarbon years BP",
                                       chronologyname = "Simon's example chronology",
                                       chroncontrols = controls)

            newSampleAges <- data.frame(predictDepths,
                                        age = colMeans(newpredictions),
                                        ageolder = colMeans(newpredictions) +
                                          apply(newpredictions, 2, sd),
                                        ageyounger = colMeans(newpredictions) -
                                          apply(newpredictions, 2, sd),
                                        agetype = "Calibrated radiocarbon years")

            stara[[1]]$collunits[[1]] <- add_chronology(stara[[1]]$collunits[[1]], newChronStara, newSampleAges)

            last_age <- samples(stara) %>%
              dplyr::filter(analysisunitid == 194633) %>%
              select(age) %>%
              unique()

            testthat::expect_true(last_age == newSampleAges$age[nrow(newSampleAges)])
          })
