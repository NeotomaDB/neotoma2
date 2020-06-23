#' An S4 class for site information from the Neotoma Paleoecology Database.
#' @include datasets.R
#' @importFrom sf sf
#' @importFrom purrr map

collunits <- setClass("collunits",
  representation(collunits = "list"),
  validity = function(object) {
    all(map(object, function(x) { class(x) == "collunit"}) %>% unlist())
  })

collunit <- setClass("collunit",
  representation(collunitid = "numeric",
    handle = "character",
    collunitname = "character",
    colldate = "Date",
    substrate = "character",
    location = "character",
    datasets = "datasets"),
  prototype(collunitid = NA_integer_,
    handle = NA_character_,
    collunitname = NA_character_,
    colldate = "Date",
    substrate = NA_character_,
    location = NA_character_,
    datasets = "datasets"),
  validity = function(object) {
    !is.na(object@datasetname) & !is.na(object@datasetid)
  })
