#' @title Build a specimen objects.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom methods new
#' @description
#' A helper function to build a specimen object from a list returned by
#' the Neotoma API call. The function is not exported, but called from
#' the get_specimens() call.
#' @param x specimen list
#' @returns A simple `specimen` object
#' @noRd
build_specimen <- function(x) {
  x <- cleanNULL(x)
  repo <- x$repository %>% cleanNULL()
  sp <- new("specimen",
            datasetid = use_na(x$datasetid, "int"),
            sampleid = use_na(x$sampleid, "int"),
            specimenid = use_na(x$specimenid, "int"),
            repository = new("repository",
                             notes = use_na(repo$notes, "char"),
                             acronym = use_na(repo$acronym, "char"),
                             repository = use_na(repo$repository, "char"),
                             repositoryid =  use_na(repo$repositoryid, "int"),
                             repositorynotes = use_na(repo$repositorynotes,
                                                      "char")),
            taxonid = use_na(x$taxonid, "int"),
            taxonname = use_na(x$taxonname, "char"),
            elementtype = use_na(x$elementtype, "char"),
            symmetry = use_na(x$symmetry, "char"),
            portion = use_na(x$portion, "char"),
            sex = use_na(x$sex, "char"),
            domesticstatus = use_na(x$domesticstatus, "char"),
            taphonomictype = use_na(x$taphonomictype, "char"),
            nisp = use_na(x$nisp, "int"),
            preservative = use_na(x$preservative, "char"),
            maturity = use_na(x$maturity, "char"),
            samplenotes = use_na(x$samplenotes, "char"))
  return(sp)
}
