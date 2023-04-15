#' @title Build a specimen objects.
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' A helper function to build a specimen object from a list returned by
#' the Neotoma API call. The function is not exported, but called from
#' the get_specimens() call.
#' @param x specimen list
#' @returns A simple `specimen` object
#' @export

build_specimen <- function(x) {

  repo <- x$repository

  new("specimen",
      datasetid = use_na(testNull(x$datasetid, NA), "int"),
      sampleid = use_na(testNull(x$sampleid, NA), "int"),
      specimenid = use_na(testNull(x$specimenid, NA), "int"),
      repository = new("repository",
                       notes = use_na(testNull(repo$notes, NA), "char"),
                       acronym = use_na(testNull(repo$acronym, NA), "char"),
                       repository = use_na(testNull(repo$repository, NA),
                         "char"),
                       repositoryid =  use_na(testNull(repo$repositoryid, NA),
                         "int"),
                       repositorynotes = use_na(testNull(repo$repositorynotes,
                         NA), "char")),
      taxonid = use_na(testNull(x$taxonid, NA), "int"),
      taxonname = use_na(testNull(x$taxonname, NA), "char"),
      elementtype = use_na(testNull(x$elementtype, NA), "char"),
      symmetry = use_na(testNull(x$symmetry, NA), "char"),
      portion = use_na(testNull(x$portion, NA), "char"),
      sex = use_na(testNull(x$sex, NA), "char"),
      domesticstatus = use_na(testNull(x$domesticstatus, NA), "char"),
      taphonomictype = use_na(testNull(x$taphonomictype, NA), "char"),
      nisp = use_na(testNull(x$nisp, NA), "int"),
      preservative =use_na(testNull(x$preservative, NA), "char"),
      maturity = use_na(testNull(x$maturity, NA), "char"),
      samplenotes = use_na(testNull(x$samplenotes, NA), "char")
  )

}
