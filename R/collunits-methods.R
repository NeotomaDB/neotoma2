# Start "c" methods
#' @title c Method - Combine objects, including NULL
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @title c Method for NULL values
#' @param x NULL object
#' @param y sites/collunits object
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x ="missingORNULL", y) {
            y
          })

#' @title  Show the collection unit information
#' @param object collunits object
#' @export
setMethod(f = "show",
          signature = signature(object = "collunits"),
          definition = function(object) {
            result <- purrr::map(object@collunits, function(x) {
              as.data.frame(x)
              }) %>%
              bind_rows()
            print(result)
          })

#' @title  Add a chronology to a collectionunit
#' @param object collunit object
#' @export
setMethod(f = "add_chronology",
          signature = signature(object = "collunit", x = "chronology", y = "data.frame"),
          definition = function(object, x, y) {
            result <- as.data.frame(object)

            print(result)
          })


#' @title  Show the collection unit information
#' @param object collunit object
#' @export
setMethod(f = "show",
          signature = signature(object = "collunit"),
          definition = function(object) {
            result <- as.data.frame(object)

            print(result)
          })

#' @title Get or remove sites by numeric index
#' @param x The collunits object
#' @param i The numeric index
setMethod(f = "[",
          signature = signature(x = "collunits", i = "numeric"),
          definition = function(x, i) {
            new("collunits", collunits = x@collunits[i])
          })

#' @title  Slicer
#' @param x collunits object
#' @param i iteration in collunits list
#' @description Obtain one of the elements within a collunits list
#' @export
setMethod(f = "[[",
          signature = signature(x = "collunits", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("collunit", x@collunits[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("collunit", x@collunits[[z]])
              })
              out <- new("collunits", collunits = out)
            }
            return(out)
          })

#' @title Get slot names
#' @param x A collection unit object.
#' @description Get all names for named elements within a `collunit` object.
#' @export
setMethod(f = "names",
          signature = signature(x = "collunit"),
          definition = function(x) {
            slotNames(x)
          })

#' @title  Insert collunit
#' @param x collunits object
#' @param i iteration in collunits list
#' @param value The value to be used
#' @description Obtain one of the elements within a collunits list
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "collunits"),
          definition = function(x, i, value) {
            collunitset <- x@collunits
            collunitset[[i]] <- value
            out <- new("collunits", collunits = collunitset)
            return(out)
          })


#' @title Assign collunit field by numeric index
#' @param x The collunit object.
#' @param i The column indicator.
#' @param value The value to be used.
setMethod(f = "[<-",
          signature = signature(x = "collunit", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @title Assign collunit field by numeric index
#' @param x The collunit object.
#' @param i The column indicator.
#' @param value The value to be used.
setMethod(f = "[<-",
          signature = signature(x = "collunit", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title Assign collunit field by numeric index
#' @param x The collunit object.
#' @param name name of the slot.
#' @param value The value to be used.
setMethod(f = "$<-",
          signature = signature(x = "collunit"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })


#' @title  $
#' @param x collunit object
#' @param name name of the slot
#' @description Obtain slots of a collunit without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "collunit"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for collunits
#' @param x collunits object
#' @param name name of the slot
#' @description Obtain slots of a collunit without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "collunits"),
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
          signature = signature("collunit"),
          definition = function(x) {
            data.frame(collectionunitid = x@collectionunitid,
                       handle = x@handle,
                       colldate = x@colldate,
                       location = x@location,
                       waterdepth = x@waterdepth,
                       collunittype = x@collunittype,
                       collectiondevice = x@collectiondevice,
                       defaultchronology = x@defaultchronology,
                       collectionunitname = x@collectionunitname,
                       depositionalenvironment = x@depositionalenvironment)
          })

#' @title  as.data.frame collunits
#' @param x collunits object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("collunits"),
          definition = function(x) {
            x@collunits %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method collunits
#' @export
#' @param x collunits object
setMethod(f = "length",
          signature = signature(x = "collunits"),
          definition = function(x) {
            length(x@collunits)
          })

#' @title c Method - Combine collunits objects
#' @param x collunits object 1
#' @param y collunits object 2
#' @export
setMethod(f = "c",
          signature = signature(x = "collunits"),
          definition = function(x, y) {
            if (class(y) == "collunits") {
              out <- new("collunits",
                         collunits = unlist(c(x@collunits,
                                          y@collunits),
                                        recursive = FALSE))
            } else if (class(y) == "collunit") {
              collunitset <- c(x@collunits, y)
              out <- new("collunits", collunits = collunitset)
            }
            return(out)
          })

#' @title write CSV
#' @param x collunits object
#' @param ... Additional parameters associated with the call.
#' @export
setMethod(f = "write.csv",
          signature = "collunits",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })
