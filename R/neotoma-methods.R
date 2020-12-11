#' An S4 class for site information from the Neotoma Paleoecology Database.
#' @import sf
#' @importFrom purrr map

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
                       !is.na(object@datasetname) & !is.na(object@datasetid)
                     })

#' An S4 class for site information from the Neotoma Paleoecology Database.
#' @import sf
#' @importFrom purrr map

collunits <- setClass("collunits",
                      representation(collunits = "list"),
                      validity = function(object) {
                        all(map(object, function(x) { class(x) == "collunit"}) %>% 
                              unlist())
                      })

#' An S4 class for site information from the Neotoma Paleoecology Database.
#'
#' @import sf

site <- setClass("site",
                 representation(siteid = "numeric",
                                sitename = "character",
                                location = "sfg",
                                description = "character",
                                notes = "character",
                                collunits = "collunits"),
                 prototype(siteid = NA_integer_,
                           sitename = NA_character_,
                           location = NULL,
                           description = NA_character_,
                           notes = NA_character_,
                           collunits = NULL),
                 validity = function(object) {
                   !is.na(object@sitename) & !is.na(object@siteid)
                 })

#' An S4 class for multi-site information from the Neotoma Paleoecology Database.
#' @import sf

sites <- setClass("sites",
                  representation(sites = "site"))
