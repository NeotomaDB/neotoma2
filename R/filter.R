#' @title filter
#' @import sf
#' @import dplyr
#' @import purrr
#' @import stringr
#' @param x A site, dataset or download.
#' @param ... arguments to filter by: lat, long, 
#' elev, datasettype
#' @export

filter <- function(x, ...) {
  UseMethod("filter")
}

#' @export
filter.sites <- function(x, ...) {  # nolint
  # Naming the Dots
  cl <- as.list(match.call())
  cl[1] <- NULL
  cl$x <- NULL
  
  calls_list <- c()
  
  for (i in seq_len(length(cl))) {
    txt <- paste0(cl[[i]], collapse = "")
    if (str_detect(txt, "lat")) {
      # Appending latitude call
      my_list <- list(lat = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
    if (str_detect(txt, "long")) {
      # Appending longitude call
      my_list <- list(long = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
    if (str_detect(txt, "elev")) {
      # Appending elevation call
      my_list <- list(elev = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
    
    
    if (str_detect(txt, "datasettype")) {
      # Appending datasettype call
      my_list <- list(datasettype = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
    
    if (str_detect(txt, "ageolder")) {
      # Appending ageolder call
      my_list <- list(ageolder = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
    
    if (str_detect(txt, "ageyounger")) {
      # Appending ageyounger call
      my_list <- list(ageyounger = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
  }
  
  for (i in names(calls_list)) {
    if (!(i %in% c("lat", "long", "elev", "datasettype", "loc", "ageolder", "ageyounger")))
      message(paste0(i, " is not a valid parameter.
       It will not be used for filtering"))
  }
  sites <- c()
  
  for (i in seq_len(length(x))) {
    coll_units <- c()
    datasets <- c()
    for (j in seq_len(length(x[[i]]@collunits))) {
      datasets_list <- c()
      
      datasets_call <- x[[i]]@collunits[[j]]@datasets
      
      for (k in seq_len(length(datasets_call))) {
        datasets_lat <- c()
        datasets_long <- c()
        datasets_elev <- c()
        datasets_type <- c()
        datasets_older <- c()
        datasets_younger <- c()
        
        # Check for type
        if ("datasettype" %in% names(calls_list)) {
          # Getting the datasettype
          datasettype <- datasets_call[[k]]@datasettype # nolint
          dataset <- datasets_call[[k]]
          
          if (eval(calls_list$datasettype) == TRUE) {
            datasets_type <- append(datasets_type, dataset)
          }else{
            datasets_type <- append(datasets_type, 0)
          }
        }
        
        # Check for lat
        if ("lat" %in% names(calls_list)) {
          lat <- sf::st_coordinates(x@sites[[i]]@geography)[, 2][1] # nolint
          dataset <- datasets_call@datasets[[k]]
          if (eval(calls_list$lat) == TRUE) {
            datasets_lat <- append(datasets_lat, dataset)
          }else{
            datasets_lat <- append(datasets_lat, 0)
          }
        }
        
        # Check for long
        if ("long" %in% names(calls_list)) {
          long <- sf::st_coordinates(x@sites[[i]]@geography)[,1][1] # nolint
          dataset <- datasets_call@datasets[[k]]
          if (eval(calls_list$long) == TRUE) {
            datasets_long <- append(datasets_long, dataset)
          }else{
            datasets_long <- append(datasets_long, 0)
          }
        }
        
        # Check for elev
        if ("elev" %in% names(calls_list)) {
          elev <- x@sites[[i]]@altitude # nolint
          dataset <- datasets_call@datasets[[k]]
          if (eval(calls_list$elev) == TRUE) {
            datasets_elev <- append(datasets_elev, dataset)
          }else{
            datasets_elev <- append(datasets_elev, 0)
          }
        }
        
        # # Check for ages
        # def_chron <- x[[i]]@collunits[[j]]@defaultchronology
        # 
        # # ageboundolder
        # if ("ageolder" %in% names(calls_list)) {
        #   ageolder <- x[[i]]@collunits[[j]]@chronologies[[1]]@ageboundolder # nolint
        #   ageolder <- if (!is.na(ageolder)) {
        #     datasets_old <- datasets_call@datasets
        #     if (eval(calls_list$ageolder) == TRUE) {
        #       datasets_older <- c(datasets_older, datasets_old)
        #       datasets_older <- Reduce(intersect, datasets_older)
        #     }else{
        #       datasets_older <- c(datasets_older, 0)
        #       datasets_older <- Reduce(intersect, datasets_older)
        #     }
        #   }
        # }
        
        # Check for age older
        if ("ageolder" %in% names(calls_list)) {
          ageolder <- datasets_call[[k]]@age_range_old
          ageolder <- if (!is.na(ageolder)) {
            datasets_old <- datasets_call[[k]]
            if (eval(calls_list$ageolder) == TRUE) {
              datasets_older <- append(datasets_older, datasets_old)
            }else{
              datasets_older <- append(datasets_older, 0)
            }
          }
        }
        
        # Check for age younger
        if ("ageyounger" %in% names(calls_list)) {
          ageyounger <- datasets_call[[k]]@age_range_young
          ageyounger <- if (!is.na(ageyounger)) {
            datasets_young <- datasets_call[[k]]
            if (eval(calls_list$ageyounger) == TRUE) {
              datasets_younger <- append(datasets_younger, datasets_young)
            }else{
              datasets_younger <- append(datasets_younger, 0)
            }
          }
        }
        
        
        datasets_names <- list(lat = datasets_lat,
                               long = datasets_long,
                               elev = datasets_elev,
                               datasettype = datasets_type,
                               ageolder = datasets_older,
                               ageyounger = datasets_younger)
        
        if (!("ageyounger" %in% names(calls_list))) {
          datasets_names$ageyounger <- NULL
        }
        
        if (!("ageolder" %in% names(calls_list))) {
          datasets_names$ageolder <- NULL
        }
        
        if (!("datasettype" %in% names(calls_list))) {
          datasets_names$datasettype <- NULL
        }
        
        if (!("lat" %in% names(calls_list))) {
          datasets_names$lat <- NULL
        }
        
        if (!("long" %in% names(calls_list))) {
          datasets_names <- purrr::list_modify(datasets_names, "long" = NULL)
        }
        if (!("elev" %in% names(calls_list))) {
          datasets_names <- purrr::list_modify(datasets_names, "elev" = NULL)
        }
        
        ch_dataset <- Reduce(intersect, datasets_names)
        
        datasets <- append(datasets, ch_dataset)
        
        if(length(ch_dataset) == 0){
          ch_dataset <- "do not append"
        }
        
        if (class(ch_dataset) == "list") {
          datasets_list <- new("datasets", datasets = ch_dataset)
          
        }else{
          datasets_list <- NULL
        }
        
        if (!is.null(datasets_list)) {
          collunit <- new("collunit", 
                          collectionunitid = x[[i]]@collunits[[j]]@collectionunitid,
                          notes = x[[i]]@collunits[[j]]@notes,
                          handle = x[[i]]@collunits[[j]]@handle,
                          colldate = x[[i]]@collunits[[j]]@colldate,
                          location = x[[i]]@collunits[[j]]@location,
                          waterdepth = x[[i]]@collunits[[j]]@waterdepth,
                          gpslocation = sf::st_as_sf(sf::st_sfc()),
                          collunittype = NA_character_,
                          collectiondevice = NA_character_,
                          collectionunitname = NA_character_,
                          depositionalenvironment = NA_character_,
                          datasets = datasets_list,
                          chronologies = new("chronologies", chronologies = list()))
          
          coll_units <- append(coll_units, collunit)
          if(class(coll_units) != "collunits") {
            coll_units <- new("collunits", collunits = coll_units)
          }
        }
      }
      
      if (!is.null(coll_units)) {
        new_site <- new("site",
                        siteid = x@sites[[i]]@siteid,
                        sitename = x@sites[[i]]@sitename,
                        geography = x@sites[[i]]@geography,
                        altitude = x@sites[[i]]@altitude,
                        description = "description",
                        notes = NA_character_,
                        collunits = coll_units)
        sites <- append(sites, new_site)
      }
    }
  }
  if (!is.null(sites)) {
    sites <- new("sites", sites = sites)
  }
  
  if (exists("sites")) {
    return(sites)
  }else{
    return(message("No datasets match your conditions."))
  }
}