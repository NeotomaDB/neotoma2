#' An S4 class for site information from the Neotoma Paleoecology Database.
#'
#' @importFrom sf sf

dataset <- setClass("dataset",
  representation(datasetid = "numeric",
    datasetname = "character",
    datasettype = "character",
    notes = "character",
    site = "site"),
  prototype(datasetid = NA_integer_,
    datasetname = NA_character_,
    notes = NA_character_,
    site = NULL),
  validity = function(object) {
    !is.na(object@datasetname) & !is.na(object@datasetid)
  })
