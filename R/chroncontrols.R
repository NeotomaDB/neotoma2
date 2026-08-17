#' @title Drop chroncontrol rows that carry no information.
#' @description Chronology control tables can contain rows that are entirely
#' `NA` (malformed or placeholder controls). Columns are matched by name
#' defensively so that hand-built chronologies -- whose `chroncontrols` slot may
#' be an empty `data.frame()`, or the `data.frame(0)` default used by
#' `set_chronology()` -- pass through untouched instead of erroring on a
#' column that is not there.
#' @param df A chroncontrols data.frame.
#' @returns The data.frame with all-NA control rows removed.
#' @noRd
drop_empty_controls <- function(df) {
  cols <- intersect(c("depth", "thickness", "agelimityounger", "agelimitolder",
                      "chroncontrolid", "chroncontrolage", "chroncontroltype"),
                    names(df))
  if (length(cols) == 0 || nrow(df) == 0) {
    return(df)
  }
  keep <- !Reduce(`&`, lapply(df[cols], is.na))
  df[keep, , drop = FALSE]
}

#' @title Recover information about the chron controls for a collectionunit.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @description For all sites that includes collection units with chronologies
#' return the chronological controls that are used in building the chronology.
#' @param x sites object
#' @returns data.frame with chronological controls
#' @export
setMethod(f = "chroncontrols",
  signature = "sites",
  definition = function(x) {
    output <- map(x@sites, function(y) chroncontrols(y)) %>%
      bind_rows()
    return(output)
  }
)

#' @title Recover information about the chron controls for a collectionunit.
#' @description For a site that includes collection units with chronologies
#' return the chronological controls that are used in building the chronology.
#' @importFrom dplyr select everything
#' @param x site object
#' @returns data.frame with chronological controls
#' @export
setMethod(f = "chroncontrols",
  signature = "site",
  definition = function(x) {
    siteid <- as.data.frame(x)$siteid
    chronset <- chroncontrols(chronologies(x))
    # A site with no chronologies still needs a `siteid` column of the right
    # type, otherwise bind_rows() across sites mixes typed and untyped frames.
    if (nrow(chronset) == 0) {
      chronset$siteid <- integer(0)
    } else {
      chronset$siteid <- siteid
    }
    chronset <- chronset %>%
      select(siteid, everything())
    return(chronset)
  }
)

#' @title Recover the chron controls for a set of chronologies.
#' @description For a `chronologies` object return the chronological controls
#' used in building each chronology, labelled by chronology.
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @param x chronologies object
#' @returns data.frame with chronological controls
#' @export
setMethod(f = "chroncontrols",
  signature = "chronologies",
  definition = function(x) {
    # Return the full column set rather than the 0x0 frame bind_rows() would
    # give for an empty list: callers (chroncontrols,site) append to it.
    if (length(x@chronologies) == 0) {
      return(data.frame(chronologyid = integer(),
                        chronologyname = character(),
                        depth = numeric(),
                        thickness = numeric(),
                        agelimityounger = numeric(),
                        agelimitolder = numeric(),
                        chroncontrolid = integer(),
                        chroncontrolage = numeric(),
                        chroncontroltype = character()))
    }
    output <- map(x@chronologies, function(y) {
      # A controls table that has already been through chroncontrols() once --
      # the round-trip the "not so simple workflow" vignette does, editing the
      # table and handing it back to set_chronology() -- still carries the
      # label columns. Drop them before relabelling so the result is
      # idempotent rather than growing chronologyid.1, chronologyname.1, ...
      controls <- y@chroncontrols
      labels <- c("siteid", "chronologyid", "chronologyname")
      controls <- controls[, !names(controls) %in% labels, drop = FALSE]
      data.frame(chronologyid = y@chronologyid,
                 chronologyname = y@chronologyname,
                 controls)
    }) %>%
      bind_rows()
    return(drop_empty_controls(output))
  }
)

#' @title Recover the chron controls for a single chronology.
#' @description For a `chronology` object return the chronological controls
#' used in building it.
#' @param x chronology object
#' @returns data.frame with chronological controls
#' @export
setMethod(f = "chroncontrols",
  signature = "chronology",
  definition = function(x) {
    return(drop_empty_controls(x@chroncontrols))
  }
)
