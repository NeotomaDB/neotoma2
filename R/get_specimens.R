#' @title get_specimens
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import geojsonsf
#' @importFrom methods new
#' @description
#' Information for Specimens
#' @param x Use a single datasetid
#' @return The function returns a specimens list
#' @examples \dontrun{
#' # To find all datasets with a min altitude of 12 and a max altitude of 25:
#' my_specimens <- get_specimens(19832)
#' }
#' @export
get_specimens <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_specimens", x)
  } else {
    UseMethod("get_specimens", NA)
  }
}


#' @title Get Dataset Numeric
#' @param x Use a single number to extract site information
#' @param ... Additional parameters to get_specimens
#' @export
get_specimens.numeric <- function(x) {
  use_na <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }
  
  if (length(x) > 0) {
    datasetid <- paste0(x, collapse = ",")
  }
  
  
  
  base_url <- paste0("data/specimens/", datasetid)
  result <- neotoma2::parseURL(base_url)
  
  # Getting specimens data
  sps <- result$data %>%
    cleanNULL()
  
  sp_index <- purrr::map(sps, function(x) {
    data.frame(datasetid = x$datasetid)}) %>%
    dplyr::bind_rows() 
  
  ds <- get_datasets(x)
  ids <- getids(ds, order = FALSE)
  
  
  # Table to check ids of dataset object
  
  
  #specimen <- map(result$data, build_specimen)
  
  #specimen <- new("specimens", specimens = specimen)
  
  my_sites_list <- c()
  siteids <- c()
  
  # Move from get_downloads to a different helper function script
  check_match <- function(sp_row, ids) {
    apply(ids, 1, function(x) sum(sp_row == x))
  }
  
  my_sites_list <- c()
  
  for (i in 1:length(sps)) {
    if (length(my_sites_list) == 0) {
      my_site <- ds[i]
      my_sites_list <- c(my_sites_list, my_site)
      
    } else {
      ids <- getids(my_sites_list, order = FALSE)
      matches <- check_match(sp_index[i,], ids)
      
      if (max(matches) == 0) {
        my_site <- ds[i]
        my_sites_list <- c(my_sites_list, my_site)
        
      } else if (max(matches) == 1) {
        # We are adding a specimens somewhere
        # Retrieve site and collunit IDs from sitelist
        st <- match(ids$siteid[which.max(matches)], unique(ids$siteid))
        
        cuids <- ids %>%
          dplyr::filter(.data$siteid == unique(ids$siteid)[st], .preserve = TRUE)
        
        cuid <- which(unique(cuids$collunitid) == ids$collunitid[which.max(matches)])
        
        
        dsids <- cuids %>%
          dplyr::filter(.data$collunitid == unique(cuids$collunitid)[cuid], .preserve = TRUE)
        
        dsid <- which(unique(dsids$datasetid) == sp_index$datasetid[i])
        
        newsp <- build_specimen(sps[i])
        datasets <- my_sites_list[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]]
        
        datasets@specimens@specimens <- c(datasets@specimens@specimens,
                                          newsp)
        
        my_sites_list[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]] <- datasets
        
        
      }
    }
  }
  return(my_sites_list)
  
}
