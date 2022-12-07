#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Helper function to build a specimen
#' @param x specimen list
#' @return list parsed into specimen
#' @export
#' @examples \dontrun{
#' # To build dataset from API call:
#' build_specimen(x)
#' }
#'
build_specimen <- function(x) {

  new("specimen",
      datasetid = use_na(testNull(x$datasetid, NA), "int"),
      sampleid = use_na(testNull(x$sampleid, NA), "int"),
      specimenid = use_na(testNull(x$specimenid, NA), "int"),
      repository = new("repository",
                       notes = use_na(testNull(x$repository$notes, NA), "char"),
                       acronym = use_na(testNull(x$repository$acronym, NA), "char"),
                       repository = use_na(testNull(x$trepository$repository, NA), "char"),
                       repositoryid =  use_na(testNull(x$repository$repositoryid, NA), "int"),
                       repositorynotes = use_na(testNull(x$repository$repositorynotes, NA), "char")),
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
