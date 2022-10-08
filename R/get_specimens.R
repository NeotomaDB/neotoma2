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
get_specimens <- function(x = NA) {
  if (!missing(x)) {
    UseMethod("get_specimens", x)
  }
}


#' @title Get Specimen Numeric
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
  
  # Move from get_downloads to a different helper function script
  check_match <- function(sp_row, ids) {
    apply(ids, 1, function(x) sum(sp_row == x))
  }
  
  for (i in 1:length(sps)) {
    
    ids <- getids(ds, order = FALSE)
    matches <- check_match(sp_index[i,], ids)
    
    if (max(matches) == 1) {
      # Retrieve IDs for site and collectionunit based on datasetID
      st <- match(ids$siteid[which.max(matches)], unique(ids$siteid))
      
      cuids <- ids %>%
        dplyr::filter(.data$siteid == unique(ids$siteid)[st], .preserve = TRUE)
      
      cuid <- which(unique(cuids$collunitid) == ids$collunitid[which.max(matches)])
      
      # Filter based on datasetID
      dsids <- cuids %>%
        dplyr::filter(.data$collunitid == unique(cuids$collunitid)[cuid], .preserve = TRUE)
      
      dsid <- which(unique(dsids$datasetid) == sp_index$datasetid[i])
      
      newsp <- build_specimen(sps[[i]])
      
      # Attach built specimen slot to datasets
      
      datasets <- ds[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]]
      
      datasets@specimens@specimens <- c(datasets@specimens@specimens,
                                        newsp)
      
      datasets@samples@samples <- ds[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]]@samples@samples
      
      ds[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]] <- datasets
      
    }
  }
  return(ds)
  
}


#' @title Get Specimen Sites
#' @param x Use a single number to extract site information
#' @param ... Additional parameters to get_specimens
#' @export
get_specimens.sites <- function(x) {
  
  output <- getids(x) %>%
    dplyr::select(.data$datasetid) %>%
    stats::na.omit() %>%
    unique() %>%
    unlist()
  
  output <- get_specimens(x = output)
  
  return(output)
}