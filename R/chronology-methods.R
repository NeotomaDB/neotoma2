# Show methods
#' @title  Slicer
#' @param x chronologies object
#' @param i iteration in chronologies list
#' @description Obtain one of the elements within a chronologies list
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

#' @title  $
#' @param x chronology object
#' @param name name of the slot
#' @description Obtain slots of a chronology without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "chronology"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for chronologies
#' @param x chronologies object
#' @param name name of the slot
#' @description Obtain slots of a chronology without using at-mark
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

#' @title  as.data.frame chronology
#' @param x chronology object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("chronology"),
          definition = function(x) {
            data.frame(chronologyid = x@chronologyid,
                       notes = x@notes,
                       agemodel = x@agemodel,
                       ageboundolder = x@ageboundolder,
                       ageboundyounger = x@ageboundyounger,
                       isdefault = x@isdefault,
                       dateprepared = ifelse(is.null(x@dateprepared), NA, x@dateprepared),
                       modelagetype = x@modelagetype,
                       chronologyname = x@chronologyname)
          })

#' @title  as.data.frame datasets
#' @param x chronologies object
#' @importFrom purrr map
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("chronologies"),
          definition = function(x) {
            x@chronologies %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method chronologies
#' @export
#' @param x chronologies object
setMethod(f = "length",
          signature = signature(x = "chronologies"),
          definition = function(x) {
            length(x@chronologies)
          })

#' @title c Method - Combine chronologies objects
#' @param x chronologies object 1
#' @param y chronologies object 2
#' @export
setMethod(f = "c",
          signature = signature(x = "chronologies"),
          definition = function(x, y) {
            tryCatch(
              expr = {
                new("chronologies",
                    chronologies = unlist(c(x@chronologies,
                                            y@chronologies), recursive = FALSE))
              },
              error = function(e){
                stop("Use `get_downloads()` to fill chronologies details. Current `sites` object
                   comes from `get_sites()` or `get_datasets()` which does not have chronology's 
                   detail")
              })
            
          })

#' @title write CSV
#' @param x chronologies object
#' @param ... Additional parameters associated with the call.
#' @export
setMethod(f = "write.csv",
          signature = "chronologies",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })
