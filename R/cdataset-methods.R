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
            database = "character",
            doi = "ANY",
            datasettype = "character",
            age_range_old = "numeric",
            age_range_young = "numeric",
            notes = "character",
            pi_list = "ANY",
            taxa_table = "ANY",
            analyst = "ANY"),
  # Set the default values for the slot
  prototype = list(datasetid = NA_integer_,
                   database = NA_character_,
                   doi = list(),
                   datasettype = NA_character_,
                   age_range_old =  NA_integer_,
                   age_range_young =  NA_integer_,
                   notes = NA_character_,
                   pi_list = list(),
                   taxa_table = data.frame(),
                   analyst = list()),
)

#' @title S4 class for datasets information
#' @description The grouped class for datasets from
#'  the Neotoma Paleoecology Database.
datasets <- setClass(
  # Set the name for the class
  "datasets",
  slots = c(datasets = "list"))

# Start "Show Method" for all Neotoma Objects
#' @title Show Dataset Method
#' @param object dataset object
setMethod(f = "show",
          signature = "dataset",
          definition = function(object) {
            print(data.frame(dataset.id = object@datasetid,
                             database = object@database,
                             datasettype = object@datasettype,
                             age_range_old =  object@age_range_old,
                             age_range_young =  object@age_range_young,
                             notes = object@notes), row.names = FALSE)
          })

#' @title Show Datasets object as a dataframe
#' @param object datasets object
setMethod(f = "show",
          signature = "datasets",
          definition = function(object) {
            map(object@datasets, function(y) {
              df <- data.frame(dataset.id = y@datasetid,
                               database = y@database,
                               datasettype = y@datasettype,
                               age_range_old =  y@age_range_old,
                               age_range_young =  y@age_range_young,
                               notes = y@notes)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @title  Slicer
#' @param x datasets object
#' @param i iteration in datasets list
#' @description Obtain one of the elements within a datasets list
#' @export
setMethod(f = "[[",
          signature = signature(x = "datasets", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("dataset", x@datasets[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("dataset", x@datasets[[z]])
              })
              out <- new("datasets", datasets = out)
            }
            return(out)
          })

#' @title  $
#' @param x dataset object
#' @description Obtain slots of a dataset without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "dataset"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for datasets
#' @param x datasets object
#' @description Obtain slots of a site without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "datasets"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @title  as.data.frame site
#' @param x site object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("dataset"),
          definition = function(x) {
            data.frame(dataset.id = x@datasetid,
                       database = x@database,
                       datasettype = x@datasettype,
                       age_range_old =  x@age_range_old,
                       age_range_young =  x@age_range_young,
                       notes = x@notes)
          })

#' @title  as.data.frame datasets
#' @param x datasets object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("datasets"),
          definition = function(x) {
            x@datasets %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method datasets
#' @export
#' @param x datasets object
setMethod(f = "length",
          signature = signature(x = "datasets"),
          definition = function(x) {
            length(x@datasets)
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

#' @title write CSV
#' @param x datasets object
#' @export
setMethod(f = "write.csv",
          signature = "datasets",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })