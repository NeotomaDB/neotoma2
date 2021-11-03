#' @title check_args
#' @description Internal function to check passed arguments.
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @param cl called arguments.
#'    Arguments are going to be called by match_call inside:
#'    \code{\link{get_sites}} 
#'    \code{\link{get_datasets}}
#'    \code{\link{get_downloads}}
#' @return A list with two components:
#'  \item{flag}{Returns a 0 if everything's fine, a 1 if there's a problem.}
#'  \item{message}{A list of error messages.}
#' @references
#' Neotoma Project Website: http://www.neotomadb.org

check_args <- function(cl) {

  error <- list(flag = 0,
                message = list())
  
  # get_sites argument checks
  if('sitename' %in% names(cl)){
    if(!is.character(cl$sitename)){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("Sitename should be a character")
    }
  }

  if("altmin" %in% names(cl)){
    if(!is.numeric(cl$altmin)){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("altmin should be a number")
    }
  }

  if("altmax" %in% names(cl)){
    if(!is.numeric(cl$altmax)){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("altmax should be a number")
    }
  }

  if(("altmax" %in% names(cl)) & ("altmin" %in% names(cl))){
    if(cl$altmax<cl$altmin){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("altmax cannot be smaller than altmin")
    }
  }

  if(("loc" %in% names(cl))){
    if(!is.character(cl$loc)){
      if(is.numeric(cl$loc)){
        if(length(cl$loc) != 4){
          error$flag <- 1
          error$message[[length(error$message) + 1]] <- paste0("loc must be a geojson string or a 4 coordinate array")
        }
      }
      }
    }

  
  return(list(cl,error))

}