#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Helper function to bulld a dataset
#' @param x dataset list
#' @return list parsed into datasets
#' @export
#' @examples \dontrun{
#' # To build dataset from API call:
#' build_dataset(x)
#' }
#'
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
      select(.data$depth, .data$thickness,
             .data$agelimitolder, .data$chroncontrolid,
             .data$agelimityounger, .data$chroncontrolage,
             .data$chroncontroltype)
    
    chron_table <- rbind(chron_table, df_sample) %>%
      distinct()
    
    new("chronology",
        chronologyid = use_na(testNull(check_chron$chronologyid, NA), "int"),
        notes = use_na(testNull(check_chron$chronology$notes, NA), "char"),
        contact = use_na(testNull(check_chron$chronology$contact, NA), "char"),
        agemodel = use_na(testNull(check_chron$chronology$agemodel, NA), "char"),
        ageboundolder = use_na(testNull(check_chron$chronology$agerange$ageboundolder, NA), "int"),
        ageboundyounger = use_na(testNull(check_chron$chronology$agerange$ageboundyounger, NA), "int"),
        isdefault = use_na(as.numeric(testNull(check_chron$chronology$isdefault, NA)), "int"),
        dateprepared = as.Date(testNull(check_chron$chronology$dateprepared, NA)),
        modelagetype = use_na(testNull(check_chron$chronology$modelagetype, NA), "char"),
        chronologyname = use_na(testNull(check_chron$chronology$chronologyname,NA), "char"),
        chroncontrols = chron_table)
}
