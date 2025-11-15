#' @rdname sub-sub
setMethod(f = "[[",
          signature = signature(x = "chronologies", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("chronology", x@chronologies[[i]])
            } else {
              out <- map(i, function(z) {
                new("chronology", x@chronologies[[z]])
              })
              out <- new("chronology", chronologies = out)
            }
            return(out)
          })

#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "chronology"),
          definition = function(x, name) {
            slot(x, name)
          })


#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "chronologies"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @rdname cash-set
setMethod(f = "$<-",
          signature = signature(x = "chronology"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("chronology"),
          definition = function(x) {
            data.frame(chronologyid = as.character(x@chronologyid),
                       notes = x@notes,
                       agemodel = x@agemodel,
                       ageboundolder = x@ageboundolder,
                       ageboundyounger = x@ageboundyounger,
                       isdefault = x@isdefault,
                       dateprepared = x@dateprepared,
                       modelagetype = x@modelagetype,
                       chronologyname = x@chronologyname)
          })

#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("chronologies"),
          definition = function(x) {
            x@chronologies %>% map(as.data.frame) %>% bind_rows()
          })

#' @rdname length
setMethod(f = "length",
          signature = signature(x = "chronologies"),
          definition = function(x) {
            length(x@chronologies)
          })

#' @rdname c
setMethod(f = "c",
          signature = signature(x = "chronologies"),
          definition = function(x, y) {
            if ("chronology" %in% class(y)) {
              y <- new("chronologies", chronologies = list(y))
            }
            tryCatch(
              new("chronologies",
                  chronologies = unlist(c(x@chronologies,
                                          y@chronologies),
                                        recursive = FALSE)),
                     error = function(e) {
                       stop("Use `get_downloads()` to fill chronologies details.
                            Current `sites` object comes from `get_sites()` or
                            `get_datasets()` which does not have chronology
                            detail")
                     })
          })

#' @title Change the default age model for a record.
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @param x A chronologies object.
#' @param n The particular chronology to be used as the default.
#' @returns `chronologies` object with a new defaulted `chronology`
#' @md
#' @export
setMethod(f = "set_default",
          signature = signature(x = "chronologies"),
          definition = function(x, n) {
            assert_that(class(x) == "chronologies")
            chron_set <- as.data.frame(x)
            assert_that(n %in% chron_set$chronologyid,
                        msg = "The new default chronology 
                        must be a valid chronologyid
                        within the chronologies.")
            which_replace <- chron_set$chronologyid == n
            replacingmodel <- chron_set$modelagetype[which_replace]
            chronout <- map(seq_len(length(x)), function(y) {
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