

#' @title S4 class for collunits information
collunit <- setClass(
  # Set the name for the class
  "collunit",
  slots = c(collectionunitid = "numeric",
            handle = "character",
            collectiondevice = "character",
            collectionunitname = "character",
            collunittype = "character",
            waterdepth = "numeric",
            colldate = "Date",
            depositionalenvironment = "character",
            location = "character",
            gpslocation = "sf",
            notes = "character",
            datasets = "datasets",
            chronologies = "chronologies"),
  prototype = list(collectionunitid = NA_integer_,
                   handle = NA_character_,
                   collectiondevice = NA_character_,
                   collectionunitname = NA_character_,
                   collunittype = NA_character_,
                   waterdepth = NA_integer_,
                   colldate = as.Date(character(0)),
                   depositionalenvironment = NA_character_,
                   location = NA_character_,
                   gpslocation = sf::st_sf(sf::st_sfc()),
                   notes = NA_character_,
                   datasets = NULL,
                   chronologies = NULL))

#' An S4 class for Neotoma Collection Units
#' @description Holds Collection unit information
#'  from the Neotoma Paleoecology Database.
#' @importFrom purrr map
collunits <- setClass("collunits",
                      representation(collunits = "list"),
                      validity = function(object) {
                        all(map(object@collunits,
                        function(x) {
                          class(x) == "collunit"
                        }) %>%
                        unlist())
                      })




# Start "c" methods
#' @title c Method - Combine objects, including NULL
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @title c Method for NULL values
#' @param x NULL object
#' @param y sites/datasets object
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x ="missingORNULL", y) {
            y
          })
