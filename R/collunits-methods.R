#' @md
utils::globalVariables(c(".", "element", "taxonid", "symmetry", "taxongroup",
                         "elementtype", "variablename", "ecologicalgroup", "element", "taxonid"))
# Start "c" methods
#' @title c Method - Combine objects, including NULL
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @title Add a new chronology into an existing collectionunit.
#' @param object A collection unit object
#' @param x A chronology object generated using \code{set_chronology()}
#' @param y A data.frame of sample ages, with required columns:
#'   `"analysisunitid"`, `"age"`, `"agetype"`, `"ageolder"`, and `"ageyounger"`.
#' @returns `chronologies` with new added chronology
#' @description Given a collunit, add a new chronology object to the unit
#' with both the chronology metadata and the age information (as `y`)
#' @details When undertaking analysis we may wish to add a new chronology to
#' existing records within Neotoma. To do this we must first build the
#' chronology, but also link it to existing analysis units within the
#' collection unit.
#' For examples from this function, see the
#' [Complex Workflows](https://open.neotomadb.org/EPD_binder/complex_workflow.html)
#' documentation online.
#' @export
setMethod(f = "add_chronology",
          signature = signature(object = "collunit",
                                x = "chronology",
                                y = "data.frame"),
          definition = function(object, x, y) {
            existingIds <- as.data.frame(object@chronologies)$chronologyid
            existinganalysisIds <- purrr::map(object@datasets@datasets,
                                              function(x) {
                                                map(x@samples@samples,
                                                    function(y) {
                                                      y$analysisunitid
                                                    }) %>%
                                                  unlist()
                                              })
            
            if (x$chronologyid %in% existingIds) {
              stop("There is already a chronology with the same ID as your 
                    new chronology.  Please change the new chronologyid.")
            }
            
            assertthat::assert_that(all(c("analysisunitid", "age", 
                                          "agetype", "ageolder",
                                          "ageyounger") %in% colnames(y)),
                                    msg = "The data.frame for sample ages must
                                           contain the columns analysisunitid,
                                           age, agetype, ageolder and ageyounger.")
            
            if (!any(unlist(existinganalysisIds) %in% y$analysisunitid)) {
              stop("There is no overlap between the existing analysis units and
                    the anaysis units in your new sample ages data.frame.")
            } else if (!all(unlist(existinganalysisIds) %in% y$analysisunitid)) {
              warning("Not all of the existing analysis units are represented in
                       the new sample ages data.frame. Analysis units without
                       ages will have NA values assigned.")
            }
            
            object@datasets@datasets <- map(object@datasets@datasets,
                                            function(z) {
                                              z@samples@samples <- map(z@samples@samples, function(a) {
                                                auid <- a@analysisunitid
                                                if (auid %in% y$analysisunitid) {
                                                  sampleagerow <- y %>% dplyr::filter(analysisunitid == auid)
                                                  
                                                    a@ages <- data.frame(age = sampleagerow$age,
                                                                         agetype = sampleagerow$agetype,
                                                                         ageolder = sampleagerow$ageolder,
                                                                         ageyounger = sampleagerow$ageyounger,
                                                                         chronologyid = x$chronologyid,
                                                                         chronologyname = x$chronologyname) %>%
                                                      rbind(., a@ages)
                                                  
                                                }
                                                return(a)
                                              })
                                              return(z)
                                            })
            
            object@chronologies <- c(object@chronologies, x)
            
            if (x@isdefault == 1) {
              object@chronologies <- set_default(object@chronologies,
                                                 x$chronologyid)
            }
            return(object)
          })

#' @title c Method for NULL values
#' @param x NULL object
#' @param y sites/collunits object
#' @returns concatenated `collunits` object
#' @export
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x = "missingORNULL", y) {
            y
          })

#' @title  Show the collection unit information
#' @param object collunits object
#' @return null used for side effects. Printing a data.frame
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


#' @title  Show the collection unit information
#' @param object collunit object
#' @return null used for side effects. Printing a data.frame
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
#' @description Retrieve sites by numeric index
#' @return null used for side effects. Printing a data.frame
setMethod(f = "[",
          signature = signature(x = "collunits", i = "numeric"),
          definition = function(x, i) {
            new("collunits", collunits = x@collunits[i])
          })

#' @title  Slicer
#' @param x collunits object
#' @param i iteration in collunits list
#' @description Obtain one of the elements within a collunits list
#' @returns sliced `collunits` object
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
#' @returns NULL. Shows the names of the slots
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
#' @returns Modified `collunits`
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
#' @returns sliced element
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
#' @returns sliced value
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
#' @returns assign a new value to a slot
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
#' @returns null prints element of a slot
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
#' @returns null prints element of a slot
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
#' @description show as dataframe 
#' @returns data.frame object with a collection units metadata
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
#' @returns data.frame of multiple collection units metadata.
#' @export
setMethod(f = "as.data.frame",
          signature = signature("collunits"),
          definition = function(x) {
            x@collunits %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method collunits
#' @export
#' @param x collunits object
#' @returns length of a `collunits` object
setMethod(f = "length",
          signature = signature(x = "collunits"),
          definition = function(x) {
            length(x@collunits)
          })

#' @title c Method - Combine collunits objects
#' @param x collunits object 1
#' @param y collunits object 2
#' @importFrom methods is
#' @returns concatenated collection units without duplicates
#' @export
setMethod(f = "c",
          signature = signature(x = "collunits"),
          definition = function(x, y) {
            if (is(y, "collunits")) {
              out <- new("collunits",
                         collunits = unlist(c(x@collunits,
                                              y@collunits),
                                            recursive = FALSE))
            } else if (is(y, "collunit")) {
              collunitset <- c(x@collunits, y)
              out <- new("collunits", collunits = collunitset)
            }
            return(out)
          })

#' @title write CSV
#' @param x collunits object
#' @param ... Additional parameters associated with the call.
#' @returns null side effect for saving a CSV file.
#' @export
setMethod(f = "write.csv",
          signature = "collunits",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })
