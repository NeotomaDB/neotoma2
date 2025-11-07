#' @title c Method - Combine objects, including NULL
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @title Add a new chronology into an existing collectionunit.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter
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
#' @md
#' @export
setMethod(f = "add_chronology",
          signature = signature(object = "collunit",
                                x = "chronology",
                                y = "data.frame"),
          definition <- function(object, x, y) {
            existingIds <- as.data.frame(object@chronologies)$chronologyid
            existinganalysisIds <- map(object@datasets@datasets,
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
            assert_that(all(c("analysisunitid", "age", "agetype", "ageolder",
                              "ageyounger") %in% colnames(y)),
                        msg = "The data.frame for sample ages must
                              contain the columns analysisunitid,
                              age, agetype, ageolder and ageyounger.")
            if (!any(unlist(existinganalysisIds) %in% y$analysisunitid)) {
              stop("There is no overlap between the existing analysis units and
                    the anaysis units in your new sample ages data.frame.")
            } else if (!all(unlist(existinganalysisIds)
                            %in% y$analysisunitid)) {
              warning("Not all of the existing analysis units are 
                          represented in the new sample ages data.frame. 
                          Analysis units without ages will have NA
                          values assigned.")
            }
            object@datasets@datasets <-
              map(object@datasets@datasets,
                  function(z) {
                    z@samples@samples <-
                      map(z@samples@samples,
                          function(a) {
                            auid <- a@analysisunitid
                            if (auid %in% y$analysisunitid) {
                              sampleagerow <- y %>%
                                filter(analysisunitid == auid)
                              a@ages <-
                                data.frame(age = sampleagerow$age,
                                           agetype = sampleagerow$agetype,
                                           ageolder = sampleagerow$ageolder,
                                           ageyounger = sampleagerow$ageyounger,
                                           chronologyid = x$chronologyid,
                                           chronologyname =
                                           x$chronologyname) %>%
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

#' @rdname show
#' @export
setMethod(f = "show",
          signature = signature(object = "collunits"),
          definition = function(object) {
            result <- map(object@collunits, function(x) {
              as.data.frame(x)
            }) %>%
              bind_rows()
            print(result)
          })

#' @rdname show
#' @export
setMethod(f = "show",
          signature = signature(object = "collunit"),
          definition = function(object) {
            result <- as.data.frame(object)
            print(result)
          })

#' @rdname sub
#' @export
setMethod(f = "[",
          signature = signature(x = "collunits", i = "numeric"),
          definition = function(x, i) {
            new("collunits", collunits = x@collunits[i])
          })

#' @rdname sub-sub
#' @export
setMethod(f = "[[",
          signature = signature(x = "collunits", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("collunit", x@collunits[[i]])
            } else {
              out <- map(i, function(z) {
                new("collunit", x@collunits[[z]])
              })
              out <- new("collunits", collunits = out)
            }
            return(out)
          })

#' @rdname names
#' @export
setMethod(f = "names",
          signature = signature(x = "collunit"),
          definition = function(x) {
            slotNames(x)
          })

#' @rdname sub-subset
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "collunits"),
          definition = function(x, i, value) {
            collunitset <- x@collunits
            collunitset[[i]] <- value
            out <- new("collunits", collunits = collunitset)
            return(out)
          })

#' @rdname subset
#' @export
setMethod(f = "[<-",
          signature = signature(x = "collunit", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @rdname subset
#' @export
setMethod(f = "[<-",
          signature = signature(x = "collunit", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @rdname cash-set
#' @export
setMethod(f = "$<-",
          signature = signature(x = "collunit"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @rdname cash
#' @export
setMethod(f = "$",
          signature = signature(x = "collunit"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @rdname cash
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

#' @rdname as.data.frame
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

#' @rdname as.data.frame
#' @export
setMethod(f = "as.data.frame",
          signature = signature("collunits"),
          definition = function(x) {
            x@collunits %>% map(as.data.frame) %>% bind_rows()
          })

#' @rdname length
#' @export
setMethod(f = "length",
          signature = signature(x = "collunits"),
          definition = function(x) {
            length(x@collunits)
          })

#' @rdname c
#' @export
setMethod(f = "c",
          signature = signature(x = "collunits"),
          definition = function(x, y) {
            if (is(y, "collunits")) {
              cu <- unlist(c(x@collunits, y@collunits),
                           recursive = FALSE)
              out <- new("collunits",
                         collunits = cu) %>%
                clean()
            } else if (is(y, "collunit")) {
              collunitset <- c(x@collunits, y)
              collunitset <- neotoma2::clean(collunitset)
              out <- new("collunits", collunits = collunitset)
            }
            return(out)
          })