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
  
  # TODO pass fix_null to its own script
  fix_null <- function(x) {
    for (i in seq_len(length(x))) {
      if (is.null(x[[i]])) {
        x[[i]] <- NA
      } else {
        if (class(x[[i]]) == "list") {
          x[[i]] <- fix_null(x[[i]])
        }
      }
    }
    return(x)
  }
  # Contact Information
  #contact <- length(y$chronology$chronology$contact)
  contact_list <- list()
  #for (k in seq_len(length(contact))) {
  #  if (!is.na(y$chronology$chronology$contact)) {
  #    cn <- y$chronology$chronology$contact[[k]]$contactname
  #    contact_list <- c(contact_list, cn)
  #  }
  #}
  if(length(x$chronology$chronology) > 1){
    check_chron <- x$chronology$chronology[[1]]
  } else{
    check_chron <- x$chronology$chronology
  }
  if (!is.na(check_chron)) {
  # Chroncontrols
  df <- x$chronology$chroncontrols %>%
    map(function(y) {
      as.data.frame(y)
    }) %>%
    bind_rows()
  
  chron_table <- data.frame()
  df_sample <- df %>%
    select(depth, thickness, agelimitolder, chroncontrolid,
           agelimityounger, chroncontrolage, chroncontroltype)
  chron_table <- rbind(chron_table, df_sample) %>%
    distinct()
  
  new("chronology",
      chronologyid = use_na(x$chronology$chronologyid, "int"),
      notes = use_na(x$chronology$chronology$notes, "char"),
      contact = contact_list,
      agemodel = use_na(x$chronology$chronology$agemodel, "char"),
      ageboundolder = use_na(testNull(x$chronology$chronology$agerange$ageboundolder, NA), "int"),
      ageboundyounger = use_na(testNull(x$chronology$chronology$agerange$ageboundyounger, NA), "int"),
      isdefault = use_na(as.numeric(testNull(x$chronology$chronology$isdefault, NA)), "int"),
      dateprepared = as.Date(x$chronology$chronology$dateprepared),
      modelagetype = use_na(testNull(x$chronology$chronology$modelagetype, NA), "char"),
      chronologyname = use_na(testNull(x$chronology$chronology$chronologyname,NA), "char"),
      chroncontrols = chron_table) 
  }else{
    new("chronology",
        chronologyid = NA_integer_,
        notes = NA_character_,
        contact = list(),
        agemodel = NA_character_,
        ageboundolder = NA_integer_,
        ageboundyounger = NA_integer_,
        isdefault = NA_integer_,
        dateprepared = as.Date(character(0)),
        modelagetype = NA_character_,
        chronologyname = NA_character_,
        chroncontrols = data.frame()) 
  } 
}