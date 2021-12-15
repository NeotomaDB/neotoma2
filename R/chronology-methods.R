#' @title S4 class for chronologies information
#' @description The grouped class for chronologies
#'  frrom the Neotoma Paleoecology Database.
chronology <- setClass(
  # Set the name for the class
  "chronology",
  
  # Define the slots
  slots = c(chronologyid = "numeric",
            notes = "character",
            contact = "ANY",
            agemodel = "character",
            ageboundolder = "numeric",
            ageboundyounger = "numeric",
            isdefault = "numeric",
            dateprepared = "Date",
            modelagetype = "character",
            chronologyname = "character",
            chroncontrols = "ANY"),
  
  # Set the default values for the slot
  prototype = list(chronologyid = NA_integer_,
                   notes = NA_character_,
                   contact = list(),
                   agemodel = NA_character_,
                   ageboundolder = NA_integer_,
                   ageboundyounger = NA_integer_,
                   isdefault = NA_integer_,
                   dateprepared = as.Date(character(0)),
                   modelagetype = NA_character_,
                   chronologyname = NA_character_,
                   chroncontrols = data.frame()),
)

#' @title S4 class for chronologies information
#' @description The grouped class for chronologies
#'  from the Neotoma Paleoecology Database.
chronologies <- setClass(
  "chronologies",
  slots = c(chronologies = "list"),
  # Validity functions
  validity = function(object) {
    all(object@chronologies %>%
          lapply(class) %>%
          unlist(recursive = FALSE) ==  "chronology")
  })


# Show methods
#' @title  Slicer
#' @param x chronologies object
#' @param i iteration in chronologies list
#' @description Obtain one of the elements within a chronologies list
#' @export
setMethod(f = "[[",
          signature = signature(x = "chronologies", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("chronology", x@chronologies[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("chronology", x@chronologies[[z]])
              })
              out <- new("chronology", chronologies = out)
            }
            return(out)
          })

#' @title  $
#' @param x chronology object
#' @description Obtain slots of a chronology without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "chronology"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for chronologies
#' @param x chronologies object
#' @description Obtain slots of a chronology without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "chronologies"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @title  as.data.frame chronology
#' @param x chronology object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("chronology"),
          definition = function(x) {
            data.frame(chronologyid = x@chronologyid,
                       notes = x@notes,
                       agemodel = x@agemodel,
                       ageboundolder = x@ageboundolder,
                       ageboundyounger = x@ageboundyounger,
                       isdefault = x@isdefault,
                       dateprepared = x@date,
                       modelagetype = x@modelagetype,
                       chronologyname = x@chronologyname)
          })

#' @title  as.data.frame datasets
#' @param x chronologies object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("chronologies"),
          definition = function(x) {
            x@chronologies %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method chronologies
#' @export
#' @param x chronologies object
setMethod(f = "length",
          signature = signature(x = "chronologies"),
          definition = function(x) {
            length(x@chronologies)
          })

#' @title c Method - Combine chronologies objects
#' @param x chronologies object 1
#' @param y chronologies object 2
#' @export
setMethod(f = "c",
          signature = signature(x = "chronologies"),
          definition = function(x, y) {
            new("chronologies",
                chronologies = unlist(c(x@chronologies,
                                    y@chronologies), recursive = FALSE))
          })

#' @title write CSV
#' @param x chronologies object
#' @export
setMethod(f = "write.csv",
          signature = "chronologies",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })