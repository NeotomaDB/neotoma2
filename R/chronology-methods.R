#' @md
#' @title Extract
#' @param x chronologies object
#' @param i iteration in chronologies list
#' @description Obtain one of the elements within a `chronologies` list
#' either by element order or by element name.
#' @returns selected `chronology` object
#' @export
setMethod(f = "[[",
          signature = signature(x = "chronologies", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("chronology", x@chronologies[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("chronology", x@chronologies[[z]])
              })
              out <- new("chronology", chronologies = out)
            }
            return(out)
          })

#' @title  Extract
#' @param x chronology object
#' @param name name of the slot
#' @description Extract chronology metadata by slot name.
#' @returns value of the `slot` name
#' @export
setMethod(f = "$",
          signature = signature(x = "chronology"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title Replace part of an object
#' @param x A chronology object
#' @param name The name of the chronology slot.
#' @param value A value to be assigned to the chronology slot.
#' @description Assign values to slots within a chronology object.
#' @returns reassigned `chronology` object
#' @export
setMethod(f = "$<-",
          signature = signature(x = "chronology"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @title Extract
#' @param x chronologies object
#' @param name name of the slot
#' @description Obtain chronology slots across all chronology elements within
#' a chronologies object.
#' @returns A multiple `chronologies`
#' @export
setMethod(f = "$",
          signature = signature(x = "chronologies"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @title Create a data.frame from a chronology object.
#' @param x chronology object
#' @description Convert all slots within a chronology to a data.frame.
#' @importFrom lubridate as_date
#' @returns data.frame
#' @export
setMethod(f = "as.data.frame",
          signature = signature("chronology"),
          definition = function(x) {
            data.frame(chronologyid = as.character(x@chronologyid),
                       notes = x@notes,
                       agemodel = x@agemodel,
                       ageboundolder = x@ageboundolder,
                       ageboundyounger = x@ageboundyounger,
                       isdefault = x@isdefault,
                       dateprepared =
                        lubridate::as_date(ifelse(is.null(x@dateprepared),
                          NA, x@dateprepared)),
                       modelagetype = x@modelagetype,
                       chronologyname = x@chronologyname)
          })

#' @title  as.data.frame chronologies
#' @param x chronologies object
#' @importFrom purrr map
#' @description Convert all slots within each chronology within a
#' chronologies object to a data.frame.
#' @returns data.frame with chronologies metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature("chronologies"),
          definition = function(x) {
            x@chronologies %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method chronologies
#' @export
#' @returns integer describing length
#' @param x chronologies object
setMethod(f = "length",
          signature = signature(x = "chronologies"),
          definition = function(x) {
            length(x@chronologies)
          })

#' @title c Method - Combine chronologies objects
#' @param x chronologies object 1
#' @param y chronologies object 2
#' @returns concatenated `chronologies`
#' @export
setMethod(f = "c",
          signature = signature(x = "chronologies"),
          definition = function(x, y) {
            if ("chronology" %in% class(y)) {
              y <- new("chronologies", chronologies = list(y))
            }
            tryCatch(
                expr = {
                  new("chronologies",
                      chronologies = unlist(c(x@chronologies,
                                              y@chronologies),
                                            recursive = FALSE))
                },
                error = function(e) {
                  stop("Use `get_downloads()` to fill chronologies details.
                        Current `sites` object comes from `get_sites()` or
                        `get_datasets()` which does not have chronology
                        detail")
                })
          })

#' @title write CSV
#' @param x chronologies object
#' @param ... Additional parameters associated with the call.
#' @returns null, called for side effects
#' @export
setMethod(f = "write.csv",
          signature = "chronologies",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })


#' @title Change the default age model for a record.
#' @param x A chronologies object.
#' @param n The particular chronology to be used as the default.
#' @returns `chronologies` object with a new defaulted `chronology`
#' @importFrom purrr map
setMethod(f = "set_default",
          signature = signature(x = "chronologies"),
          definition = function(x, n) {
            assertthat::assert_that(class(x) == "chronologies")

            chron_set <- as.data.frame(x)

            assertthat::assert_that(n %in% chron_set$chronologyid,
              msg = "The new default chronology must be a valid chronologyid
                     within the chronologies.")

            which_replace <- chron_set$chronologyid == n
            replacingmodel <- chron_set$modelagetype[which_replace]

            chronout <- purrr::map(seq_len(length(x)), function(y) {
              if (x@chronologies[[y]]$chronologyid == n) {
                x@chronologies[[y]]@isdefault <- TRUE
              }
              if (x@chronologies[[y]]$chronologyid != n &
                  x@chronologies[[y]]$modelagetype == replacingmodel) {
                x@chronologies[[y]]@isdefault <- FALSE
              }
              return(x@chronologies[[y]])
            })

            return(new("chronologies", chronologies = chronout))
          })
