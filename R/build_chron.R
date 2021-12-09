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
  
  if(length(x)>1){
    new_chrons <- x %>%
      map(function(y) {
        
        # Contact Information
        contact <- length(y$chronology$chronology$contact)
        contact_list <- c()
        for (k in seq_len(length(contact))) {
          if (!is.na(y$chronology$chronology$contact)) {
            cn <- y$chronology$chronology$contact[[k]]$contactname
            contact_list <- c(contact_list, cn)
          }
        }
        
        
        # Chroncontrols
        df <- y$chronology$chroncontrols %>%
          map(function(x) {
            as.data.frame(x)
          }) %>%
          bind_rows()
        
        chron_table <- data.frame()
        df_sample <- df %>%
          select(depth, thickness, agelimitolder, chroncontrolid,
                 agelimityounger, chroncontrolage, chroncontroltype)
        chron_table <- rbind(chron_table, df_sample) %>%
          distinct()
        
        new("chronology",
            chronologyid = use_na(y$chronology$chronologyid, "int"),
            notes = use_na(y$chronology$chronology$notes, "char"),
            contact = contact_list,
            agemodel = use_na(y$chronology$chronology$agemodel, "char"),
            ageboundolder = use_na(x$chronology$chronology$agerange$ageboundolder, "int"),
            ageboundyounger = use_na(x$chronology$chronology$agerange$ageboundyounger, "int"),
            is_default = use_na(as.numeric(x$chronology$chronology$isdefault), "int"),
            dateprepared = as.Date(y$chronology$chronology$dateprepared),
            modelagetype = use_na(y$chronology$chronology$modelagetype, "char"),
            chronologyname = use_na(y$chronology$chronology$chronologyname, "char"),
            chroncontrols = chron_table) 
      })
    
   
  }else{
    new("chronology",
        chronologyid = NA_integer_,
        notes = NA_character_,
        contact = list(), #contact_list,
        agemodel = NA_character_,
        agerange = list(), #agerange_list,
        dateprepared = as.Date(character(0)),
        modelagetype = NA_character_,
        chronologyname = NA_character_,
        chroncontrols = data.frame()) 
  }
}