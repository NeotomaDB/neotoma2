#' @title S4 class for dataset information
#' @description The standard object class for datasets from the Neotoma Paleoecology Database.
#' @import sf
#' @importFrom purrr map
#' @importFrom dplyr bind_rows

dataset <- setClass("dataset",
                    representation(datasetid = "numeric",
                                   datasetname = "character",
                                   datasettype = "character",
                                   notes = "character"),
                    prototype(datasetid = NA_integer_,
                              datasetname = NA_character_,
                              datasettype = "character",
                              notes = NA_character_),
                    validity = function(object) {
                      !is.na(object@datasetid)
                    })


datasets <- setClass("datasets",
                     representation(datasets = "list"),
                     validity = function(object) {
                       all(object@datasets %>%
                         lapply(class) %>%
                         unlist(recursive = FALSE) ==  'dataset')
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
                               datasets = NULL),
                     validity = function(object) {
                       !is.na(object@collunitid)
                     })

#' An S4 class for Neotoma Collection Units
#' @desciption Holds Collection unit information from the Neotoma Paleoecology Database.
#' @import sf
#' @importFrom purrr map

collunits <- setClass("collunits",
                      representation(collunits = "list"),
                      validity = function(object) {
                        all(map(object@collunits, function(x) { class(x) == "collunit"}) %>%
                              unlist())
                      })

#' An S4 class for site information from the Neotoma Paleoecology Database.
#'
#' @import sf

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
                           notes = NA_character_,
                           collunits = NULL))

setMethod(f = "get_site",
          signature= signature(x = "site"),
          definition = function(x){
            slotNames(x)
          })
#' An S4 class for multi-site information from the Neotoma Paleoecology Database.
#' @import sf

sites <- setClass("sites",
                  representation(sites = "list"),
                  validity = function(object) {
                    all(map(object@sites, function(x) { class(x) == "site"}) %>%
                          unlist())
                  })
