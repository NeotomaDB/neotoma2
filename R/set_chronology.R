#' @title set chronology information for a new record.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @description Create a new chronology for a record.
#' Within Neotoma all chronologies have unique numeric
#' identifiers. Within R, because of the need to use the 
#' identifiers across objects, and because we want to avoid
#'  conflicts between naming systems, a universally unique 
#' identifier (UUID) is created for the object ID.
#' @importFrom methods is new slot<-
#' @importFrom digest digest
#' @importFrom uuid UUIDgenerate
#' @param x Object to be set as a chronology
#' @param chronologyid An optional value. Will be assigned a
#' unique identifier if not provided.
#' @param contact A contacts object, identifying the individual(s)
#' who created the chronology
#' @param agemodel A string representing the age model name, for
#' example "Crummy linear interpolation".
#' @param ageboundolder The `ageboundolder` is assigned the oldest sample
#' age rounded up to the nearest 10
#' @param ageboundyounger The `ageboundyounger` is assigned the oldest
#' sample age rounded up to the nearest 10
#' @param isdefault Defines whether the model is the default for
#' the collection unit for a particular model age type.
#' @param notes Additional notes about the chronology. For more
#' modern models, often the function call to Bacon or Bchron is added here.
#' @param dateprepared The date at which the age model was prepared.
#' @param modelagetype The age type for the model. For validation, the models
#' should be one of the valid Neotoma \code{agetypes}:
#' \url{https://api.neotomadb.org/v2.0/data/dbtables?table=agetypes}
#' @param chronologyname A valid name for the chronology.
#' @param chroncontrols A data.frame containing the chronological controls
#' for the age model.
#' @description Function to create new chronology objects for
#' personal analysis. 
#' The new object will not be uploaded to the database.
#' @returns `chronology` object
#' @md
#' @export
set_chronology <- function(x = NA,
                           chronologyid = NA_integer_,
                           notes = NA_character_,
                           contact = list(),
                           agemodel = NA_character_,
                           ageboundolder = NA_integer_,
                           ageboundyounger = NA_integer_,
                           isdefault = NA_integer_,
                           dateprepared = as.Date(character(0)),
                           modelagetype = NA_character_,
                           chronologyname = NA_character_,
                           chroncontrols = data.frame(0)) {
  function_call <- match.call()
  if (suppressWarnings(is.na(x))) {
    x <- new("chronology")
    if (is.na(x@chronologyid)) {
      if (is.na(chronologyid)) {
        hash <- digest(UUIDgenerate(), algo = "xxhash32", serialize = FALSE)
        x@chronologyid <- as.integer(strtoi(substr(hash, 1, 7), base = 16L))
      } else {
        x@chronologyid <- chronologyid
      }
      x@contact <- contact
      x@agemodel <- agemodel
      x@ageboundolder <- ceiling(ageboundolder / 10) * 10
      x@ageboundyounger <- floor(ageboundyounger / 10) * 10
      x@isdefault <- isdefault
      x@notes <- notes
      x@dateprepared <- dateprepared
      x@modelagetype <- modelagetype
      x@chronologyname <- chronologyname
      x@chroncontrols <- chroncontrols
    } else {
      if (is(x, "chronology")) {
        if (length(function_call) > 2) {
          for (i in 3:length(function_call)) {
            slot(x, names(function_call)[[i]]) <- eval(function_call[[i]])
          }
          return(x)
        } else {
          return(x)
        }
      } else {
        stop("`x` must be a chronology object if it is supplied.")
      }
    }
    return(x)
  }
}