#' @title S4 class for dataset information
#' @description The standard object class for samples
#'  from the Neotoma Paleoecology Database.
#' @import sf
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
sample <- setClass(
  # Set the name for the class
  "sample",
  # Define the slots
  slots = c(ages = "ANY",
            igsn = "character",
            datum = "ANY",
            depth = "numeric",
            sampleid = "numeric",
            thickness = "numeric",
            samplename = "character",
            sampleanalyst = "ANY",
            analysisunitid = "numeric",
            analysisunitname = "character"),
  
  # Set the default values for the slot
  prototype = list(ages = list(),
                   igsn = NA_character_,
                   datum = data.frame(),
                   depth = NA_integer_,
                   sampleid = NA_integer_,
                   thickness = NA_integer_,
                   samplename = NA_character_,
                   sampleanalyst = list(),
                   analysisunitid = NA_integer_,
                   analysisunitname = NA_character_),
)

#' @title S4 class for datasets information
#' @description The grouped class for datasets from
#'  the Neotoma Paleoecology Database.
samples <- setClass(
  # Set the name for the class
  "samples",
  slots = c(samples = "list"))

#' @title  Slicer
#' @param x samples object
#' @param i iteration in samples list
#' @description Obtain one of the elements within a samples list
#' @export
setMethod(f = "[[",
          signature = signature(x = "samples", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("sample", x@samples[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("sample", x@samples[[z]])
              })
              out <- new("samples", samples = out)
            }
            return(out)
          })

#' @title  $
#' @param x sample object
#' @description Obtain slots of a sample without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "sample"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for samples
#' @param x samples object
#' @description Obtain slots of a site without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "samples"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })


#' @title Length Method samples
#' @export
#' @param x samples object
setMethod(f = "length",
          signature = signature(x = "samples"),
          definition = function(x) {
            length(x@samples)
          })