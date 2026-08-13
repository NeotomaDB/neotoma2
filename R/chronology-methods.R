#' @title Display a `chronology` object
#' @name show
#' @export
setMethod(
  "show",
  "chronology",
  function(object) {
    
    df <- data.frame(
      chronologyid = object@chronologyid,
      chronologyname = object@chronologyname,
      agemodel = object@agemodel,
      ageboundolder = object@ageboundolder,
      ageboundyounger = object@ageboundyounger,
      dateprepared = object@dateprepared,
      modelagetype = object@modelagetype,
      isdefault = object@isdefault,
      n_controls = sum(!is.na(object@chroncontrols$chroncontrolid))
    )
    
    df <- subset(
      df,
      !(is.na(chronologyname) &
          is.na(agemodel) &
          is.na(ageboundolder) &
          is.na(ageboundyounger) &
          is.na(dateprepared) &
          is.na(modelagetype) &
          is.na(isdefault) &
          n_controls == 0)
    )
    
    if (nrow(df) > 0) {
      print(df, row.names = FALSE)
    } else {
      cat("No chronology information available.\n")
    }
  }
)
#' @title Display a `chronologies` object
#' @name show
#' @export
setMethod(
  "show",
  "chronologies",
  function(object) {
    
    if (length(object@chronologies) == 0) {
      cat("No chronology information available.\n")
      return(invisible(NULL))
    }
    
    df <- purrr::map(object@chronologies, function(ch) {
      data.frame(
        chronologyid = ch@chronologyid,
        chronologyname = ch@chronologyname,
        agemodel = ch@agemodel,
        ageboundolder = ch@ageboundolder,
        ageboundyounger = ch@ageboundyounger,
        dateprepared = ch@dateprepared,
        modelagetype = ch@modelagetype,
        isdefault = ch@isdefault,
        n_controls = sum(!is.na(ch@chroncontrols$chroncontrolid))
      )
    }) |>
      dplyr::bind_rows()
    
    df <- subset(
      df,
      !(is.na(chronologyname) &
          is.na(agemodel) &
          is.na(ageboundolder) &
          is.na(ageboundyounger) &
          is.na(dateprepared) &
          is.na(modelagetype) &
          is.na(isdefault) &
          n_controls == 0)
    )
    
    if (nrow(df) > 0) {
      print(df, row.names = FALSE)
    } else {
      cat("No chronology information available.\n")
    }
  }
)

#' @export
setMethod(
  "chroncontrols",
  "chronologies",
  function(x) {
    
    if (length(x@chronologies) == 0) {
      return(
        data.frame(
          chronologyid = integer(),
          chronologyname = character(),
          depth = numeric(),
          thickness = numeric(),
          agelimityounger = numeric(),
          agelimitolder = numeric(),
          chroncontrolid = integer(),
          chroncontrolage = numeric(),
          chroncontroltype = character()
        )
      )
    }
    
    df <- purrr::map(x@chronologies, function(ch) {
      
      data.frame(
        chronologyid = ch@chronologyid,
        chronologyname = ch@chronologyname,
        ch@chroncontrols
      )
      
    }) |>
      dplyr::bind_rows()
    
    df <- subset(
      df,
      !(is.na(depth) &
          is.na(thickness) &
          is.na(agelimityounger) &
          is.na(agelimitolder) &
          is.na(chroncontrolid) &
          is.na(chroncontrolage) &
          is.na(chroncontroltype))
    )
    
    df
  }
)
#' @export
setMethod(
  "chroncontrols",
  "chronology",
  function(x) {
    
    df <- x@chroncontrols
    
    df <- subset(
      df,
      !(is.na(depth) &
          is.na(thickness) &
          is.na(agelimityounger) &
          is.na(agelimitolder) &
          is.na(chroncontrolid) &
          is.na(chroncontrolage) &
          is.na(chroncontroltype))
    )
    
    df
  }
)

setMethod(
  "chroncontrols",
  "site",
  function(x) {
    
    siteid <- as.data.frame(x)$siteid
    
    chronset <- chroncontrols(chronologies(x))
    
    if (nrow(chronset) == 0) {
      chronset$siteid <- integer(0)
    } else {
      chronset$siteid <- siteid
    }
    
    chronset <- dplyr::select(chronset, siteid, dplyr::everything())
    
    chronset
  }
)

#' @rdname sub-sub
#' @aliases [[,chronologies,numeric-method [[,chronologies,numeric,ANY-method
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
