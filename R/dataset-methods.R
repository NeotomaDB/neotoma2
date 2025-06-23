# Start "Show Method" for all Neotoma Objects
#' @title Show Dataset Method
#' @param object dataset object
#' @returns null - side effect, prints a `data.frame` with `dataset` metadata
setMethod(f = "show",
          signature = "dataset",
          definition = function(object) {
            print(data.frame(datasetid = as.character(object@datasetid),
                             database = object@database,
                             datasettype = object@datasettype,
                             age_range_old =  object@age_range_old,
                             age_range_young =  object@age_range_young,
                             age_units = object@age_units,
                             notes = object@notes), row.names = FALSE)
          })

#' @title Show Datasets object as a dataframe
#' @param object datasets object
#' @returns null - side effect, prints a `data.frame` with `datasets` metadata
setMethod(f = "show",
          signature = "datasets",
          definition = function(object) {
            map(object@datasets, function(y) {
              df <- data.frame(datasetid = as.character(y@datasetid),
                               database = y@database,
                               datasettype = y@datasettype,
                               age_range_old =  y@age_range_old,
                               age_range_young =  y@age_range_young,
                               age_units = y@age_units,
                               notes = y@notes)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @title  Slicer
#' @param x datasets object
#' @param i iteration in datasets list
#' @description Obtain one of the elements within a datasets list
#' @returns sliced `dataset` object
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

#' @title Get slot names
#' @param x A dataset object.
#' @description Get all names for named elements within a `dataset` object.
#' @returns `list` with all names of `dataset` slots
#' @export
setMethod(f = "names",
          signature = signature(x = "dataset"),
          definition = function(x) {
            slotNames(x)
          })

#' @title  Insert dataset
#' @param x datasets object
#' @param i iteration in datasets list
#' @param value The value to be used
#' @description Obtain one of the elements within a datasets list
#' @returns One `dataset` slot's value 
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "datasets"),
          definition = function(x, i, value) {
            datasetset <- x@datasets
            datasetset[[i]] <- value
            out <- new("datasets", datasets = datasetset)
            return(out)
          })


#' @title Assign dataset field by numeric index
#' @param x The dataset object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `dataset` slot with new assigned character value
setMethod(f = "[<-",
          signature = signature(x = "dataset", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @title Assign dataset field by numeric index
#' @param x The dataset object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `dataset` slot with new assigned numeric value
setMethod(f = "[<-",
          signature = signature(x = "dataset", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title Assign dataset field by numeric index
#' @param x The dataset object.
#' @param name name of the slot.
#' @param value The value to be used.
#' @returns Assign new `dataset` by numeric index
setMethod(f = "$<-",
          signature = signature(x = "dataset"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })



#' @title Get or remove datasets by numeric index
#' @param x The datasets object
#' @param i The numeric index
#' @returns Get or remove `datasets` by numeric index
setMethod(f = "[",
          signature = signature(x = "datasets", i = "numeric"),
          definition = function(x, i) {
            new("datasets", datasets = x@datasets[i])
          })

#' @title  $
#' @param x dataset object
#' @param name name of the slot
#' @description Obtain slots of a dataset without using at-mark
#' @returns Obtain a `dataset`'s `slot` value using $
#' @export
setMethod(f = "$",
          signature = signature(x = "dataset"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for datasets
#' @param x datasets object
#' @param name name of the slot.
#' @description Obtain slots of a dataset without using at-mark
#' @returns Obtain a `datasets`' `slot` value using $  
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

#' @title  as.data.frame dataset
#' @param x dataset object
#' @description show as dataframe as prep to save as csv
#' @returns `data.frame` with `dataset` metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature("dataset"),
          definition = function(x) {
            data.frame(datasetid = as.character(x@datasetid),
                       database = x@database,
                       datasettype = x@datasettype,
                       age_range_old =  x@age_range_old,
                       age_range_young =  x@age_range_young,
                       age_units = x@age_units,
                       notes = x@notes)
          })

#' @title  as.data.frame datasets
#' @param x datasets object
#' @description show as dataframe as prep to save as csv
#' @returns `data.frame` with `datasets` metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature("datasets"),
          definition = function(x) {
            x@datasets %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method datasets
#' @export
#' @returns `int` that showcases the length of a `datasets` object
#' @param x datasets object
setMethod(f = "length",
          signature = signature(x = "datasets"),
          definition = function(x) {
            length(x@datasets)
          })

#' @title c Method - Combine datasets objects
#' @param x datasets object 1
#' @param y datasets object 2
#' @returns concatenated `datasets` object
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
#' @param ... Additional parameters associated with the call.
#' @returns null -side effect for printing a CSV file
#' @export
setMethod(f = "write.csv",
          signature = "datasets",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })
