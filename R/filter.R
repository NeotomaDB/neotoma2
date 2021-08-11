#' @title filter
#' @import sf
#' @import dplyr
#' @import purrr
#' @param x A site, dataset or download.
#' @param ... arguments to filter by: latmin, latmax, longmin, longmax, elevmin, elevmax, datasettype.
#' @export

filter <- function(x, ...) { 
  UseMethod('filter')
}

#' @export
filter.default <- function(x, ...) {
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  
  error_check <- check_args(cl)
  
  for(i in as.list(names(cl))){
    if(!(i %in% c('latmin', 'latmax', 'longmin', 'longmax', 'elevmin', 'elevmax', 'datasettype', 'x')))
      message(paste0(i, " not a valid parameter. Will not be used for filtering"))
  }
  sites <- c()
  
  for(i in 1: length(x@sites)){
    coll_units <- c()
    for(j in 1:length(x@sites[[i]]@collunits@collunits)){
      
      datasets_list <- c()
      datasets <- c()
      for(k in 1:length(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets)){
        
        datasets_latmin <- c()
        datasets_latmax <- c()
        datasets_longmin <- c()
        datasets_longmax <- c()
        datasets_elevmin <- c()
        datasets_elevmax <- c()
        datasets_type <- c()

        # Check for type
        if('datasettype' %in% names(cl)){
          if(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]@datasettype == cl$datasettype){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            datasets_type <- append(datasets_type, dataset)
          }
        }
        
        # Check for latmin
        if('latmin' %in% names(cl)){
          if(st_coordinates(x@sites[[i]]@location)[,2][1] > cl$latmin){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            datasets_latmin <- append(datasets_latmin, dataset)
          }else{
            datasets_latmin <- append(datasets_latmin, 0)
          }
        }
        # Check for latmax
        if('latmax' %in% names(cl)){
          if(st_coordinates(x@sites[[i]]@location)[,2][1] < cl$latmax){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            datasets_latmax <- append(datasets_latmax, dataset)
          }else{
            datasets_latmax <- append(datasets_latmax, 0)
          }
        }
        # Check for longmin
        if('longmin' %in% names(cl)){
          if(st_coordinates(x@sites[[i]]@location)[,1][1] > cl$longmin){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            datasets_longmin <- append(datasets_longmin, dataset)
          }else{
            datasets_longmin <- append(datasets_longmin, 0)
          }
        }
        # Check for longmax
        if('longmax' %in% names(cl)){
          if(st_coordinates(x@sites[[i]]@location)[,1][1] < cl$longmax){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            datasets_longmax <- append(datasets_longmax, dataset)
          }else{
            datasets_longmax <- append(datasets_longmax, 0)
          }
        }
        # Check for elevgmin
        if('elevmin' %in% names(cl)){
          if(x@sites[[i]]@altitude > cl$elevmin){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            datasets_elevmin <- append(datasets_elevmin, dataset)
          }else{
            datasets_elevmin <- append(datasets_elevmin, 0)
          }
        }
        # Check for elevmax
        if('elevmax' %in% names(cl)){
          if(x@sites[[i]]@altitude < cl$elevmax){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            datasets_elevmax <- append(datasets_elevmax, dataset)
          }else{
            datasets_elevmax <- append(datasets_elevmax, 0)
          }
        }
        
        
        datasets_names <- list(latmin = datasets_latmin, 
                               latmax = datasets_latmax, 
                               longmin = datasets_longmin, 
                               longmax = datasets_longmax, 
                               elevmin = datasets_elevmin, 
                               elevmax = datasets_elevmax,
                               datasettype = datasets_type)
        
        if(!("datasettype" %in% names(cl))){
          datasets_names <- purrr::list_modify(datasets_names, "datasettype" = NULL)
        }
        if(!("latmin" %in% names(cl))){
          datasets_names <- purrr::list_modify(datasets_names, "latmin" = NULL)
        }
        if(!("latmax" %in% names(cl))){
          datasets_names <- purrr::list_modify(datasets_names, "latmax" = NULL)
        }
        if(!("longmin" %in% names(cl))){
          datasets_names <- purrr::list_modify(datasets_names, "longmin" = NULL)
        }
        if(!("longmax" %in% names(cl))){
          datasets_names <- purrr::list_modify(datasets_names, "longmax" = NULL)
        }
        if(!("elevmin" %in% names(cl))){
          datasets_names <- purrr::list_modify(datasets_names, "elevmin" = NULL)
        }
        if(!("elevmax" %in% names(cl))){
          datasets_names <- purrr::list_modify(datasets_names, "elevmax" = NULL)
        }
        
        ch_dataset <- Reduce(intersect, datasets_names)
        datasets <- append(datasets, ch_dataset)
        
        #datasets <- datasets[datasets != 0]
  
        if(length(datasets) != 0){
          if(!(0 %in% datasets)){
            datasets_list <- new('datasets', datasets = datasets)}
        }else{datasets_list <- NULL}
          }
      if(!is.null(datasets_list)){
        collunit <- x@sites[[i]]@collunits@collunits
        coll_units <- append(coll_units, collunit)
        coll_units <- new('collunits', collunits = coll_units)}
    }
    if(!is.null(datasets_list)){
      new_site <- new("site",
                      siteid = x@sites[[i]]@siteid,
                      sitename = x@sites[[i]]@sitename,
                      location = x@sites[[i]]@location, 
                      altitude = x@sites[[i]]@altitude,
                      description = "description",
                      notes = NA_character_,
                      collunits = coll_units)   
      sites <- append(sites, new_site)
      
    }
    
  }
  if(!is.null(sites)){
    sites <- new('sites', sites = sites)}
  
  if(exists("sites")){
    return(sites)}else{return(message("No datasets match your conditions."))}
}