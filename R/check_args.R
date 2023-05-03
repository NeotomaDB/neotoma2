#' @title check_args
#' @description Internal function to check passed arguments.
#' @author Socorro Dominguez
#' @param cl called arguments.
#'    Arguments are going to be called by match_call inside:
#'    \code{\link{get_sites}}
#'    \code{\link{get_datasets}}
#'    \code{\link{get_downloads}}
#' @returns A list with two components:
#'  \item{flag}{Returns a 0 if everything's fine, a 1 if there's a problem.}
#'  \item{message}{A list of error messages.}
#' @references
#' Neotoma Project Website: https://www.neotomadb.org/

check_args <- function(cl) { # nolint

  error <- list(flag = 0,
                message = list())

  # get_sites argument checks
  if ("sitename" %in% names(cl)) {
    if (!is.character(cl$sitename)) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("Sitename
       should be a character")
    }
  }

  if ("altmin" %in% names(cl)) {
    if (!is.numeric(cl$altmin)) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("altmin
       should be a number")
    }
  }

  if ("altmax" %in% names(cl)) {
    if (!is.numeric(cl$altmax)) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("altmax
       should be a number")
    }
  }

  if (("altmax" %in% names(cl)) & ("altmin" %in% names(cl))) {
    if (cl$altmax < cl$altmin) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("altmax
       cannot be smaller than altmin")
    }
  }

  if (("loc" %in% names(cl))) {
    if (!is.character(cl$loc)) {
      if (is.numeric(cl$loc)) {
        if (length(cl$loc) != 4) {
          error$flag <- 1
          error$message[[length(error$message) + 1]] <- paste0("loc
           must be an sf object, a geojson string or a 4 coordinate 
           array c(xmin, xmax, ymax, ymin)")
        }
      }
      }
  }

  # Datasests accepted arguments: contactid,
  # datasettype, altmin, altmax, loc, ageyoung, ageold, ageof

  if ("contactid" %in% names(cl)) {
    if (!is.numeric(cl$contactid)) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("contactid
       should be a number")
    }
  }

  if ("datasettype" %in% names(cl)) {
    if (!is.character(cl$datasettype)) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("datasettype
       should be a character")
    }
  }

  if ("ageyoung" %in% names(cl)) {
    if (!is.numeric(cl$ageyoung)) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("ageyoung
       should be a number")
    }
  }

  if ("ageold" %in% names(cl)) {
    if (!is.numeric(cl$ageold)) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("ageold
       should be a number")
    }
  }

  if ("ageof" %in% names(cl)) {
    if (!is.numeric(cl$ageof)) {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0("ageof
       should be a number")
    }
  }
  return(list(cl, error))
}