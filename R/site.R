#' An S4 class for site information from the Neotoma Paleoecology Database.
#'
#' @importFrom sf sf

site <- setClass("site",
  representation(siteid = "numeric",
    sitename = "character",
    location = "sf",
    description = "character",
    notes = "character",
    collunits = "collunits"),
  prototype(siteid = NA_integer_,
    sitename = NA_character_,
    location = NULL,
    description = NA_character_,
    notes = NA_character_),
  validity = function(object) {
    !is.na(object@sitename) & !is.na(object@siteid)
  })
