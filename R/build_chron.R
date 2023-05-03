utils::globalVariables(c("depth", "thickness", "agelimitolder",
  "chroncontrolid", "agelimityounger", "chroncontrolage",
  "chroncontroltype"))

#' @title build_chron
#' @author Socorro Dominguez
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' A helper function to build a new chronology object from the
#' Neotoma API response.
#' @param x A chronology element from the API JSON output.
#' @returns A single `chronology` object.
#' @details This function is an internal function called from
#' `build_collunit()` to help support the translation between the JSON
#' representation of data in the API and the R implementation.
#' @export
build_chron <- function(x) {
  check_chron <- x$chronology

    # Chroncontrols
    df <- x$chronology$chroncontrols %>%
      map(function(y) {
        y <- fix_null(y)
        as.data.frame(y)
      }) %>%
      bind_rows()

    chron_table <- data.frame()
    df_sample <- df %>%
      select(depth, thickness,
             agelimitolder, chroncontrolid,
             agelimityounger, chroncontrolage,
             chroncontroltype)

    chron_table <- rbind(chron_table, df_sample) %>%
      distinct()

    # Chronologies dictionary in API is of length 1 or length 8; if length 1,
    # the chronology is an NA chronology
    if (length(check_chron$chronology) != 1) {
      ch <- check_chron$chronology
      if (length(ch$contact) == 1) {
        contact <- use_na(testNull(ch$contact, NA), "char")
      } else if (length(ch$contact) > 1) {
        contact <- ch$contact
      } else {
        contact <- use_na(testNull(ch$contact, NA), "char")
      }

      new("chronology",
        chronologyid = use_na(testNull(check_chron$chronologyid, NA), "int"),
        notes = use_na(testNull(ch$notes, NA), "char"),
        contact = contact,
        agemodel = use_na(testNull(ch$agemodel,
          NA), "char"),
        ageboundolder = use_na(testNull(ch$agerange$ageboundolder,
          NA), "int"),
        ageboundyounger = use_na(testNull(ch$agerange$ageboundyounger,
          NA), "int"),
        isdefault = use_na(as.numeric(testNull(ch$isdefault, NA)), "int"),
        dateprepared = as.Date(testNull(ch$dateprepared, NA)),
        modelagetype = use_na(testNull(ch$modelagetype, NA), "char"),
        chronologyname = use_na(testNull(ch$chronologyname, NA), "char"),
        chroncontrols = chron_table)
    } else {
      new("chronology",
          chronologyid = NA_integer_,
          notes = NA_character_,
          contact = NA_character_,
          agemodel = NA_character_,
          ageboundolder = NA_integer_,
          ageboundyounger = NA_integer_,
          isdefault = NA_integer_,
          dateprepared = as.Date(NA),
          modelagetype = NA_character_,
          chronologyname = NA_character_,
          chroncontrols = chron_table)
    }
}
