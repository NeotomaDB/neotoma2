#' @title filter
#' @importFrom utils write.csv write.table
#' @import sf
#' @param x A site, dataset or download.
#' @param latmin Minimum latitude to filter by
#' @param latmax Maximum latitude to filter by
#' @param longmin Minimum longitude to filter by
#' @param longmin Minimum longitude to filter by
#' @param elevmin Minimum elevation to filter by
#' @param elevmax Maximum elevation to filter by
#' @param type Type to filter by
#' @param ... optional arguments to pass into \code{get_dataset}.
#' @export

filter <- function(x, ...) { #latmin = NA, latmax = NA, longmin=NA, longmax = NA, elevmin = NA, elevmax = NA, type = NA,
  UseMethod('filter')
}

#' @export
filter.default <- function(x, ...) { #latmin = NA, latmax = NA, longmin=NA, longmax = NA, elevmin = NA, elevmax = NA, type = NA,
  datasets <- c()
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  
  print(names(cl))
  error_check <- check_args(cl)
  
  if('type' %in% names(cl)){  
      for(i in 1: length(x@sites)){
        for(j in 1:length(x@sites[[i]]@collunits@collunits)){
          for(k in 1:length(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets)){
            if(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]@datasettype == cl$type){
              dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
              datasets <- append(datasets, dataset)
            }
          }
        }
      }
  }
 
  datasets_list <- new('datasets', datasets = datasets)
  return(datasets_list) 
  
  
  # if(!is.na(latmin)){
  #   for(i in 1: length(x@datasets)){
  #     if(st_coordinates(x@datasets[[i]]@location)[,1] > latmin){
  #       dataset <- x@datasets[[i]]
  #       datasets <- c(datasets, dataset)
  #     }
  #   }
  # 
  # }
  # 
  # if(!is.na(latmax)){
  #   for(i in 1: length(x@datasets)){
  #     if(st_coordinates(x@datasets[[i]]@location)[,1] < latmax){
  #       print(st_coordinates(x@datasets[[i]]@location)[,1])
  #       print(latmax)
  #       dataset <- x@datasets[[i]]
  #       datasets <- c(datasets, dataset)
  #     }
  #   }
  # 
  # }
  # 
  # 
  # if(!is.na(longmin)){
  #   for(i in 1: length(x@datasets)){
  #     if(st_coordinates(x@datasets[[i]]@location)[,2] > longmin){
  #       dataset <- x@datasets[[i]]
  #       datasets <- c(datasets, dataset)
  #     }
  #   }
  # 
  # }
  # 
  # if(!is.na(longmax)){
  #   for(i in 1: length(x@datasets)){
  #     if(st_coordinates(x@datasets[[i]]@location)[,2] < longmax){
  #       dataset <- x@datasets[[i]]
  #       datasets <- c(datasets, dataset)
  #     }
  #   }
  # 
  # }
  # 
  # if(!is.na(type)){
  #   for(i in 1: length(x@datasets)){
  #     if(x@datasets[[i]]@datasettype == type){
  #       dataset <- x@datasets[[i]]
  #       datasets <- c(datasets, dataset)
  #     }
  #   }
  # 
  # }

  # if(!is.na(elevmin)){
  #   for(i in 1: length(x@datasets)){
  #     if(st_coordinates(x@datasets[[i]]@location)[,2] > longmin){
  #       dataset <- x@datasets[[i]]
  #       datasets <- c(datasets, dataset)
  #     }
  #   }
  #   
  # }
  # 
  # if(!is.na(elevmax)){
  #   for(i in 1: length(x@datasets)){
  #     if(st_coordinates(x@datasets[[i]]@location)[,2] < longmax){
  #       dataset <- x@datasets[[i]]
  #       datasets <- c(datasets, dataset)
  #     }
  #   }
  #   
  # }
  
  

}

filter.sites <- function(x, latmin = NA, latmax = NA, longmin=NA, longmax = NA, elevmin = NA, elevmax = NA, type = NA, ...) {
  datasets <- c()
  return
  }