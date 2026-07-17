#' @title build_chron
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @description
#' A helper function to build a new chronology object from the
#' Neotoma API response. This is the one place where the API's chronology field
#' names are mapped onto the slots of a `chronology`.
#'
#' The API nests a chronology twice: `x$chronology` carries the id and the
#' controls, and `x$chronology$chronology` carries the metadata.
#' @param x A chronology element from the API JSON output.
#' @returns A single `chronology` object, or `NULL` if the element carries no
#' chronology.
#' @details This function is an internal function called from
#' `parse_site()` to help support the translation between the JSON
#' representation of data in the API and the R implementation.
#' @noRd
build_chron <- function(x) {
  assert_that(is.list(x), msg = "Parsed object must be a list.")
  ch <- x$chronology
  if (is.null(ch$chronologyid)) {
    return(NULL)
  }
  meta <- ch$chronology
  older <- use_na(meta$agerange$ageboundolder, "int")
  younger <- use_na(meta$agerange$ageboundyounger, "int")
  df <- map(ch$chroncontrols, function(y) {
    as.data.frame(list(
      depth = use_na(y$depth, "int"),
      thickness = use_na(y$thickness, "int"),
      agelimityounger = use_na(y$agelimityounger, "int"),
      agelimitolder = use_na(y$agelimitolder, "int"),
      chroncontrolid = use_na(y$chroncontrolid, "int"),
      chroncontrolage = use_na(y$chroncontrolage, "int"),
      chroncontroltype = use_na(y$chroncontroltype, "char")
    ))
  }) %>%
    bind_rows()
  chron_table <- df[!duplicated(df), ]
  # Drop chroncontrol rows that carry no real chroncontrolid: those are
  # malformed/empty controls and would otherwise become phantom NA rows.
  if ("chroncontrolid" %in% names(chron_table)) {
    chron_table <- chron_table[!is.na(chron_table$chroncontrolid), ]
  }
  chronology <- set_chronology(chronologyid = use_na(ch$chronologyid, "int"),
                               notes = use_na(meta$notes, "char"),
                               contact = use_na(meta$contact, "list"),
                               agemodel = use_na(meta$agemodel, "char"),
                               ageboundolder = older,
                               ageboundyounger = younger,
                               isdefault = use_na(meta$isdefault, "bool"),
                               dateprepared = use_na(as.Date(meta$dateprepared),
                                                     "date"),
                               modelagetype = use_na(meta$modelagetype, "char"),
                               chronologyname = use_na(meta$chronologyname,
                                                       "char"),
                               chroncontrols = chron_table)
  return(chronology)
}
