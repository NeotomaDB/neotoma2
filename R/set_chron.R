#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @importFrom methods slot<-
#' @param x Object to be set as a chronoly
#' @param notes Notes of the site
#' @param contact Contact for chronology
#' @param agemodel Agemodel
#' @param ageboundyounger Younger age
#' @param isdefault Marker for default chronology
#' @param dateprepared Date when the chronology was prepared
#' @param modelagetype Model of age type
#' @param chronologyname chronology name
#' @param chroncontrols df of chronology
#' @export
#' @examples
#' \dontrun{
#' # Create a site called "My Lake", to
#' my_chron <- set_site(sitename = "My Lake",
#'                     geography = x,
#'                     description = "my lake",
#'                     altitude = 30)
#' }
#' 
set_chronology <- function(x = NA,
                           notes = NA_character_,
                           contact = list(),
                           agemodel = NA_character_,
                           ageboundolder = NA_integer_,
                           ageboundyounger = NA_integer_,
                           isdefault = NA_integer_,
                           dateprepared = as.Date(character(0)),
                           modelagetype = NA_character_,
                           chronologyname = NA_character_,
                           chroncontrols = data.frame()) {
  function_call <- match.call()
  
  if (suppressWarnings(is.na(x))) {
    x <- new("chronology")
    x@notes <- notes
    x@contact <- contact
    x@agemodel <- agemodel
    x@ageboundolder <- ageboundolder
    x@ageboundyounger <- ageboundyounger
    x@isdefault <- isdefault
    x@dateprepared <- dateprepared
    x@modelagetype <- modelagetype
    x@chronologyname <- chronologyname
    x@chroncontrols <- chroncontrols
  }
  return(x)
}