#' @title build_chron
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @description
#' A helper function to build a new chronology object from the
#' Neotoma API response.
#' @param x A chronology element from the API JSON output.
#' @returns A single `chronology` object.
#' @details This function is an internal function called from
#' `build_collunit()` to help support the translation between the JSON
#' representation of data in the API and the R implementation.
#' @noRd
build_chron <- function(...) {
  args <- list(...)
  assert_that(is.list(args), msg = "Parsed object must be a list.")
  df <- map(args$chroncontrols, function(x) {
    as.data.frame(list(
      depth = use_na(x$depth, "int"),
      thickness = use_na(x$thickness, "int"),
      agelimityounger = use_na(x$agelimityounger, "int"),
      agelimitolder = use_na(x$agelimitolder, "int"),
      chroncontrolid = use_na(x$chroncontrolid, "int"),
      chroncontrolage = use_na(x$chroncontrolage, "int"),
      chroncontroltype = use_na(x$chroncontroltype, "char")
    ))
  }) %>%
    bind_rows()
  chron_table <- df[!duplicated(df), ]
  chronology <- set_chronology(chronologyid = use_na(args$chronologyid, "int"),
                               notes = use_na(args$notes, "char"),
                               contact = use_na(args$contact, "list"),
                               agemodel = use_na(args$agemodel, "char"),
                               ageboundolder = use_na(args$ageboundolder,
                                                      "int"),
                               ageboundyounger = use_na(args$ageboundyounger,
                                                        "int"),
                               isdefault = use_na(args$isdefault, "bool"),
                               dateprepared = use_na(args$dateprepared, "date"),
                               modelagetype = use_na(args$modelagetype, "char"),
                               chronologyname = use_na(args$chronologyname,
                                                       "char"),
                               chroncontrols = chron_table)
  return(chronology)
}