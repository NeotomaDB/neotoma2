#' @title S4 class for dataset information
#' @description The standard object class for datasets
#'  from the Neotoma Paleoecology Database.
#' @import sf
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
dataset <- setClass(
                    # Set the name for the class
                    "dataset",
                    # Define the slots
                    slots = c(datasetid = "numeric",
                              datasetname = "character",
                              datasettype = "character",
                              location = "sf",
                              notes = "character",
                              taxa_table = "ANY",
                              pi_list = "ANY",
                              analyst = "ANY",
                              metadata = "ANY"),
                    # Set the default values for the slot
                    prototype = list(datasetid = NA_integer_,
                                     datasetname = NA_character_,
                                     datasettype = NA_character_,
                                     location = st_sf(st_sfc()),
                                     notes = NA_character_,
                                     taxa_table = data.frame(),
                                     pi_list = list(),
                                     analyst = list(),
                                     metadata = data.frame()),
)

#' @title S4 class for datasets information
#' @description The grouped class for datasets from
#'  the Neotoma Paleoecology Database.
datasets <- setClass(
                    # Set the name for the class
                    "datasets",
                    # Define the slots
                    slots = c(datasets = "list"),
                    # Validity functions
                    validity = function(object) {
                      all(object@datasets %>%
                            lapply(class) %>%
                            unlist(recursive = FALSE) ==  "dataset")
  })

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
            agerange = "ANY",
            dateprepared = "Date",
            modelagetype = "character",
            chronologyname = "character",
            chroncontrols = "ANY"),

  # Set the default values for the slot
  prototype = list(chronologyid = NA_integer_,
            notes = NA_character_,
            contact = list(),
            agemodel = NA_character_,
            agerange = list(),
            dateprepared = as.Date(character(0)),
            modelagetype = NA_character_,
            chronologyname = NA_character_,
            chroncontrols = data.frame()),
)

#' @title S4 class for chronologies information
#' @description The grouped class for datasets
#'  from the Neotoma Paleoecology Database.
chronologies <- setClass(
  # Set the name for the class
  "chronologies",

  # Define the slots
  slots = c(chronologies = "list"),

  # Validity functions
  validity = function(object) {
    all(object@chronologies %>%
          lapply(class) %>%
          unlist(recursive = FALSE) ==  "chronology")
  })

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
                   gpslocation = st_sf(st_sfc()),
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


# Start "Show Method" for all Neotoma Objects
#' @title Show Dataset Method
#' @param object dataset object
setMethod(f = "show",
          signature = "dataset",
          definition = function(object) {
            print(data.frame(dataset.id = object@datasetid,
                             site.name = object@datasetname,
                             lat = mean(st_coordinates(object@location)[, 2]),
                             long = mean(st_coordinates(object@location)[, 1]),
                             type = object@datasettype), row.names = FALSE)
          })

#' @title Show Datasets object as a dataframe
#' @param object datasets object
setMethod(f = "show",
          signature = "datasets",
          definition = function(object) {
            map(object@datasets, function(x) {
              df <- data.frame(dataset.id = x@datasetid,
                               site.name = x@datasetname,
                               lat = mean(st_coordinates(x@location)[, 2]),
                               long = mean(st_coordinates(x@location)[, 1]),
                               type = x@datasettype)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

# End of Show Methods

# Start "SqBrackets" Methods

#' @title Obtain one of the elements within a sites list
#' @param x sites object
#' @param i iteration in sites list
#' @description  in progress
#' @export
setMethod(f = "[[",
          signature = signature(x = "sites", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("sites", x@sites[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("site", x@sites[[z]])
              })
              out <- new("sites", sites = out)
            }
            return(out)
          })

#' @title Obtain one of the elements within a datasets list
#' @param x datasets object
#' @param i iteration in datasets list
#' @description  in progress
#' @export
setMethod(f = "[[",
          signature = signature(x = "datasets", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("datasets", x@datasets[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("datasets", x@datasets[[z]])
              })
              out <- new("datasets", sites = out)
            }
            return(out)
          })
# End "SqBrackets" Methods

# Start "length" Methods
#' @title Length Method Sites
#' @export
#' @param x sites object
setMethod(f = "length",
          signature = signature(x = "sites"),
          definition = function(x) {
            length(x@sites)
          })

#' @title Length Method Sites
#' @export
#' @param x datasets object
setMethod(f = "length",
          signature = signature(x = "datasets"),
          definition = function(x) {
            length(x@datasets)
          })

# TODO : After show method for collunits
# setMethod(f = "length",
#           signature = signature(x = "collunits"),
#           definition = function(x) {
#             length(x@collunits) # nolint
#           })

# End "length" methods

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

#' @title c Method - Combine sites objects
#' @param x sites object 1
#' @param y sites object 2
#' @export
setMethod(f = "c",
          signature = signature(x = "sites"),
          definition = function(x, y) {
            new("sites",
                sites = unlist(c(x@sites,
                                       y@sites), recursive = FALSE))
            })

#' @title c Method - Combine datasets objects
#' @param x datasets object 1
#' @param y datasets object 2
#' @export
setMethod(f = "c",
          signature = signature(x = "datasets"),
          definition = function(x, y) {
            new("datasets",
                datasets = unlist(c(x@datasets,
                                   y@datasets), recursive = FALSE))
          })

# End "c" methods

# Start writeCSV methods
setMethod(f = "write.csv",
          signature = "sites",
          definition = function(object, file, ...) {
            df1 <- map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@location)[, 2]),
                               long = mean(st_coordinates(x@location)[, 1]),
                               elev = x@altitude,
                               description = x@description)
            }) %>%
              bind_rows()
            write.csv(df1, file, ...)
          })

setMethod(f = "write.csv",
          signature = "datasets",
          definition = function(object, file, ...) {
            df1 <- map(object@datasets, function(x) {
              df <- data.frame(siteid = x@datasetid,
                               sitename = x@datasetname,
                               lat = mean(st_coordinates(x@location)[, 2]),
                               long = mean(st_coordinates(x@location)[, 1]),
                               type = x@datasettype)
            }) %>%
              bind_rows()
            write.csv(df1, file, ...)
          }
)

#' @title Convert sites object to a \code{data.frame}
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @export
# Todo Convert to as.data.frame
setGeneric("showDatasets", function(object) {
  standardGeneric("showDatasets")
})
