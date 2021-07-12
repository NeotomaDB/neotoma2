#' @title Internal function to check passed arguments.
#'
#' @description Functions \code{\link{get_sites}},
#'   \code{\link{get_datasets}} need passed arguments to be checked.
#'   \code{param_check} tells them if there's a problem.
#' @param cl called arguments.
#'    Arguments are going to be called by match_call inside
#'    \code{\link{get_sites}} or \code{\link{get_datasets}}
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @return A list with two components:
#'
#'  \item{flag}{Returns a 0 if everything's fine, a 1 if there's a problem.}
#'  \item{message}{A list of error messages.}
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' @keywords internal misc
#'
check_args <- function(cl) {

  error <- list(flag = 0,
                message = list())

  print("let's check errors")

  if("sitename" %in% names(cl)) {
    if(!is.character(cl$sitename)) {
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
      stop("altmax cannot be smaller than altmin")
    }
  }


  list(cl,error)

}