#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Helper function to build a dataset
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
        if(is.null(y$chronology$chronology$modelagetype)){
          modelagetype <- NA_character_
        }else{
          modelagetype <- use_na(y$chronology$chronology$modelagetype, "char")
        }
        if(length(y$chronology$chronology$chronologyname) == 0){
          chronologyname <- NA_character_
        }else{
          chronologyname <- use_na(y$chronology$chronology$chronologyname, "char")
        }
        
        new("chronology",
            chronologyid = use_na(y$chronology$chronologyid, "int"),
            notes = use_na(y$chronology$chronology$notes, "char"),
            contact = list(), #contact_list,
            agemodel = use_na(y$chronology$chronology$agemodel, "char"),
            agerange = list(), #agerange_list,
            dateprepared = as.Date(y$chronology$chronology$dateprepared),
            modelagetype = use_na(y$chronology$chronology$modelagetype, "char"),
            chronologyname = use_na(y$chronology$chronology$chronologyname, "char"),
            chroncontrols = data.frame()) #fix
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
        chroncontrols = data.frame()) #fix
  }
}



#     chronology_call <- result$data[[i]]$site$collectionunit$chronologies
# 
#     for (j in seq_len(length(chronology_call))) {
# 
#       chron_call <- chronology_call[[j]]$chronology
#       if (!is.na(chron_call$chronologyid)) {
#         chronologyid <- chron_call$chronologyid
#       } else {
#         chronologyid <- NA_integer_
#       }
# 
#       if (length(chron_call$chronology) > 1) {
#         check_on <- chron_call$chronology[[1]]
#       } else {
#         check_on <- chron_call$chronology
#       }
# 
#       if (!is.na(check_on)) {
# 
#         notes <- chron_call$chronology$notes
#         agemodel <- chron_call$chronology$agemodel
#         older_ <- chron_call$chronology$agerange$ageboundolder
#         younger_ <- chron_call$chronology$agerange$ageboundyounger
#         agerange_list <- c()
#         agerange_list$older <- older_
#         agerange_list$younger <- younger_
# 
#         dateprep <- as.Date(chron_call$chronology$dateprepared)
# 
#         modelagetype <- chron_call$chronology$modelagetype
# 
#         chronologyname <- chron_call$chronology$chronologyname
# 
#       # Contact Information
#       contact_length <- length(chron_call$chronology$contact)
# 
#       contact_list <- c()
# 
#         for (k in seq_len(contact_length)) {
#           if (!is.na(chron_call$chronology$contact)) {
#             cn <- chron_call$chronology$contact[[k]]$contactname
#             contact_list <- c(contact_list, cn)
#             }
#           }
# 
#       # Chroncontrols DF
# 
#       df <- chronology_call[[j]]$chronology$chroncontrols %>%
#         map(function(x) {
#           as.data.frame(x)
#         }) %>%
#         bind_rows()
# 
#       df_sample <- df %>%
#         select(depth, thickness, agelimitolder, chroncontrolid,
#         agelimityounger, chroncontrolage, chroncontroltype)
#         chron_table <- rbind(chron_table, df_sample) %>%
#         distinct()
# 
#       # End chronologies
# 
#       } else {
#         notes <- NA_character_
#         agemodel <- NA_character_
#         agerange_list <- list()
#         dateprep <- as.Date(character(0))
#         modelagetype <- NA_character_
#         chronologyname <- NA_character_
#         contact_list <- c()
#         df_sample <- data.frame()
#       }
# 
#       new_chronology <- new("chronology",
#                           chronologyid = chronologyid,
#                           notes = notes,
#                           contact = contact_list,
#                           agemodel = agemodel,
#                           agerange = agerange_list,
#                           dateprepared = dateprep,
#                           modelagetype = modelagetype,
#                           chronologyname = chronologyname,
#                           chroncontrols = df_sample)
# 
#     chronology_list <- append(chronology_list, new_chronology)
#     }
