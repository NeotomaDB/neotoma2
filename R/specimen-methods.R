# Start "Show Method" for all Neotoma Objects
#' @title Show Specimen Method
#' @param object specimen object
setMethod(f = "show",
          signature = "specimen",
          definition = function(object) {
            print(data.frame(datasetid = object@datasetid,
                             sampleid = object@sampleid,
                             specimenid = object@specimenid,
                             taxonid =  object@taxonid,
                             taxonname =  object@taxonname,
                             elementtype = object@elementtype,
                             symmetry = object@symmetry,
                             portion = object@portion,
                             sex = object@sex,
                             domesticstatus = object@domesticstatus,
                             taphonomictype = object@taphonomictype,
                             samplenotes = object@samplenotes
                             ), row.names = FALSE)
          })

#' @title Show Specimens object as a dataframe
#' @param object specimens object
setMethod(f = "show",
          signature = "specimens",
          definition = function(object) {
            map(object@specimens, function(y) {
              df <- data.frame(datasetid = y@datasetid,
                               sampleid = y@sampleid,
                               specimenid = y@specimenid,
                               taxonid =  y@taxonid,
                               taxonname =  y@taxonname,
                               elementtype = y@elementtype,
                               symmetry = y@symmetry,
                               portion = y@portion,
                               sex = y@sex,
                               domesticstatus = y@domesticstatus,
                               taphonomictype = y@taphonomictype,
                               samplenotes = y@samplenotes)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @title  Slicer
#' @param x specimens object
#' @param i iteration in specimens list
#' @description Obtain one of the elements within a specimens list
#' @export
setMethod(f = "[[",
          signature = signature(x = "specimens", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("specimen", x@specimens[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("specimen", x@specimens[[z]])
              })
              out <- new("specimens", specimens = out)
            }
            return(out)
          })

#' @title Get slot names
#' @param x A specimen object.
#' @description Get all names for named elements within a `specimen` object.
#' @export
setMethod(f = "names",
          signature = signature(x = "specimen"),
          definition = function(x) {
            slotNames(x)
          })

#' @title  Insert specimen
#' @param x specimens object
#' @param i iteration in specimens list
#' @param value The value to be used
#' @description Obtain one of the elements within a specimens list
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "specimens"),
          definition = function(x, i, value) {
            specimenset <- x@specimens
            specimenset[[i]] <- value
            out <- new("specimens", specimens = specimenset)
            return(out)
          })


#' @title Assign specimen field by numeric index
#' @param x The specimen object.
#' @param i The column indicator.
#' @param value The value to be used.
setMethod(f = "[<-",
          signature = signature(x = "specimen", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @title Assign specimen field by numeric index
#' @param x The specimen object.
#' @param i The column indicator.
#' @param value The value to be used.
setMethod(f = "[<-",
          signature = signature(x = "specimen", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title Assign specimen field by numeric index
#' @param x The specimen object.
#' @param name name of the slot.
#' @param value The value to be used.
setMethod(f = "$<-",
          signature = signature(x = "specimen"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @title Get or remove specimens by numeric index
#' @param x The specimens object
#' @param i The numeric index
setMethod(f = "[",
          signature = signature(x = "specimens", i = "numeric"),
          definition = function(x, i) {
            new("specimens", specimens = x@specimens[i])
          })

#' @title  $
#' @param x specimen object
#' @param name name of the slot
#' @description Obtain slots of a specimen without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "specimen"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for specimens
#' @param x specimens object
#' @param name name of the slot.
#' @description Obtain slots of a specimen without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "specimens"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @title  as.data.frame specimen
#' @param x specimen object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("specimen"),
          definition = function(x) {
            data.frame(datasetid = x@datasetid,
                       sampleid = x@sampleid,
                       specimenid = x@specimenid,
                       taxonid =  x@taxonid,
                       taxonname =  x@taxonname,
                       elementtype = x@elementtype,
                       symmetry = x@symmetry,
                       portion = x@portion,
                       sex = x@sex,
                       domesticstatus = x@domesticstatus,
                       taphonomictype = x@taphonomictype,
                       samplenotes = x@samplenotes)
          })

#' @title  as.data.frame specimens
#' @param x specimens object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("specimens"),
          definition = function(x) {
            x@specimens %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method specimens
#' @export
#' @param x specimens object
setMethod(f = "length",
          signature = signature(x = "specimens"),
          definition = function(x) {
            length(x@specimens)
          })

#' @title c Method - Combine specimens objects
#' @param x specimens object 1
#' @param y specimens object 2
#' @export
setMethod(f = "c",
          signature = signature(x = "specimens"),
          definition = function(x, y) {
            new("specimens",
                specimens = unlist(c(x@specimens,
                                    y@specimens), recursive = FALSE))
          })

#' @title write CSV
#' @param x specimens object
#' @param ... Additional parameters associated with the call.
#' @export
setMethod(f = "write.csv",
          signature = "specimens",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })