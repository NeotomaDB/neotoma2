#' @title c Method - Combine objects, including NULL
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @aliases add_chronology,collunit-method
#' @rdname add_chronology
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
                                dplyr::filter(.data$analysisunitid == auid)
                              sample_ages <-
                                data.frame(age = sampleagerow$age,
                                           agetype = sampleagerow$agetype,
                                           ageolder = sampleagerow$ageolder,
                                           ageyounger = sampleagerow$ageyounger,
                                           chronologyid = x$chronologyid,
                                           chronologyname =
                                           x$chronologyname)
                              a@ages <- rbind(sample_ages, a@ages)
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

#' @aliases show,collunits-method
#' @rdname show
setMethod(f = "show",
          signature = signature(object = "collunits"),
          definition = function(object) {
            result <- map(object@collunits, function(x) {
              as.data.frame(x)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @aliases show,collunit-method
#' @rdname show
setMethod(f = "show",
          signature = signature(object = "collunit"),
          definition = function(object) {
            result <- as.data.frame(object)
            print(result, row.names = FALSE)
          })

#' @aliases sub,collunits-method
#' @rdname sub
setMethod(f = "[",
          signature = signature(x = "collunits", i = "numeric"),
          definition = function(x, i) {
            new("collunits", collunits = x@collunits[i])
          })

#' @aliases sub-sub,collunits-method
#' @rdname sub-sub
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

#' @aliases names,collunit-method
#' @rdname names
setMethod(f = "names",
          signature = signature(x = "collunit"),
          definition = function(x) {
            slotNames(x)
          })

#' @aliases sub-subset,collunits-method
#' @rdname sub-subset
setMethod(f = "[[<-",
          signature = signature(x = "collunits"),
          definition = function(x, i, value) {
            collunitset <- x@collunits
            collunitset[[i]] <- value
            out <- new("collunits", collunits = collunitset)
            return(out)
          })

#' @aliases subset,collunit-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "collunit", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @aliases subset,collunit-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "collunit", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in seq_along()(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @aliases cash-set,collunit-method
#' @rdname cash-set
setMethod(f = "$<-",
          signature = signature(x = "collunit"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @aliases cash,collunit-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "collunit"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @aliases cash,collunits-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "collunits"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @aliases as.data.frame,collunit-method
#' @rdname as.data.frame
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

#' @aliases as.data.frame,collunits-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("collunits"),
          definition = function(x) {
            df <- x@collunits %>% map(as.data.frame) %>% bind_rows()
          })

#' @aliases length,collunits-method
#' @rdname length
setMethod(f = "length",
          signature = signature(x = "collunits"),
          definition = function(x) {
            length(x@collunits)
          })

#' @aliases c,collunits-method
#' @rdname c
setMethod(f = "c",
          signature = signature(x = "collunits"),
          definition = function(x, y) {
            if (is(y, "collunits")) {
              cus_l <- c(x@collunits,
                         y@collunits)
              if (is.null(names(cus_l)) || all(names(cus_l) == "")) {
                # Case 1: Remove duplicates by content
                cus_l <- cus_l[!duplicated(cus_l)]
              } else {
                # Case 2: Remove duplicates by name
                cus_l <- cus_l[!duplicated(names(cus_l))]
              }
              out <- new("collunits",
                         collunits = unlist(cus_l,
                                            recursive = FALSE))
            } else if (is(y, "collunit")) {
              collunitset <- c(x@collunits, y)
              out <- new("collunits", collunits = collunitset)
            }
            return(out)
          })