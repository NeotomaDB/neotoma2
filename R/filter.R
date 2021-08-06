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

filter <- function(x, ...) { 
  UseMethod('filter')
}

#' @export
filter.default <- function(x, ...) {
  datasets <- c()
  
  is.defined = function(x)!is.null(x)
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  
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
  
  if('latmin' %in% names(cl)){  
    for(i in 1: length(x@sites)){
      for(j in 1:length(x@sites[[i]]@collunits@collunits)){
        for(k in 1:length(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets)){
          if(st_coordinates(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]@location)[,1] > cl$latmin){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            if(!('type' %in% names(cl))){
            datasets <- append(datasets, dataset)
            }else{
              datasets_dummy <- c(dataset)
              datasets <- intersect(datasets_dummy, datasets)
            }
          }
        }
      }
    }
  }
  
  if('latmax' %in% names(cl)){  
    for(i in 1: length(x@sites)){
      for(j in 1:length(x@sites[[i]]@collunits@collunits)){
        for(k in 1:length(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets)){
          if(st_coordinates(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]@location)[,1] < cl$latmax){
            dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
            if(!('type' %in% names(cl) | 'latmin' %in% names(cl))){
              datasets <- append(datasets, dataset)
            }else{
              datasets_dummy <- c(dataset)
              datasets2 <- intersect(datasets_dummy, datasets)
            }
          }
        }
      }
    }
    if(exists("datasets2")){datasets <- datasets2}
  }

  # if('longmin' %in% names(cl)){  
  #   for(i in 1: length(x@sites)){
  #     for(j in 1:length(x@sites[[i]]@collunits@collunits)){
  #       for(k in 1:length(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets)){
  #         if(st_coordinates(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]@location)[,2] > cl$longmin){
  #           dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
  #           datasets <- append(datasets, dataset)
  #         }
  #       }
  #     }
  #   }
  # }
  # 
  # if('longmax' %in% names(cl)){  
  #   for(i in 1: length(x@sites)){
  #     for(j in 1:length(x@sites[[i]]@collunits@collunits)){
  #       for(k in 1:length(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets)){
  #         if(st_coordinates(x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]@location)[,2] < cl$longmax){
  #           dataset <- x@sites[[i]]@collunits@collunits[[j]]@datasets@datasets[[k]]
  #           datasets <- append(datasets, dataset)
  #         }
  #       }
  #     }
  #   }
  # }
  # 
  #datasets <- intersect(datasets1, datasets2, datasets3, datasets4, datasets5)
  #print("datasets")
  #print(datasets)
  
  datasets_list <- new('datasets', datasets = datasets)
  
  return(datasets_list) 
  
  


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

