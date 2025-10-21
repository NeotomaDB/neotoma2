#' @title get_sites
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom methods new
#' @param x A sites object.
#' @export
get_speleothems <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_speleothems", x)
  } else {
    UseMethod("get_speleothems", NA)
  }
}

#' @title Get Speleothem Data from a Sites object
#' @param x The numeric dataset ID from Neotoma
#' @param ... accepted arguments if numeric all_data
#' @returns The function returns either a single item of class `"try-error"`
#' describing the reason for failure (either misdefined parameters or an error
#' from the Neotoma API), or a sies object with speleothem data.
#' @examples {
#' ## Find speleothems by numeric datasetid:
#' sites <- get_seleothem(seq(1,3))
#' }
#' @export
get_speleothems.numeric <- function(x, ...) {
  if (length(x) > 0) {
    dsids <- paste0(x, collapse = ",")
  }
  sites <- get_datasets(datasetid=dsids)
  
  base_url <- paste0("data/speleothems/", dsids)
  result <- parseURL(base_url, ...)
  print(result)
  if (length(result[2]$data) > 0) {
     output <- parse_speleothem(result)
     print(output)
     return(output)
   } else {
     return(NULL)
   }
}
#' 
#' #' @title get_sites
#' #' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' #' @import gtools
#' #' @import lubridate
#' #' @import sf
#' #' @importFrom methods new
#' #' @importFrom utils URLencode
#' #' @param ... One of a set of possible query parameters discussed in details.
#' #' @returns The function returns either a single item of class `"try-error"`
#' #' describing the reason for failure (either misdefined parameters or an error
#' #' from the Neotoma API), or a table of sites, with rows corresponding to the
#' #' number of individual sites returned by the Neotoma API.
#' #' Each "site" object contains 6 parameters that can be accessed as well:
#' #' siteid, sitename, location, altitude, description,
#' #' limited collection units information.
#' #'  * `loc` An `sf` object that describes site's location.
#' #'  * `collunits` limited information on collunits
#' #' @export
#' get_sites.default <- function(...) {
#'   cl <- as.list(match.call())
#'   cl[[1]] <- NULL
#'   cl <- lapply(cl, eval, envir = parent.frame())
#'   params <- get_params("sites")
#'   if (!all(names(cl) %in% params)) {
#'     warning("Some parameters seem invalid. The current accepted parameters are: ",
#'             paste(unlist(params), collapse = ", "))
#'   }
#'   oo <- options(scipen = 9999999)
#'   on.exit(options(oo))
#' 
#'     base_url <- paste0("data/sites")
#'     result <- parseURL(base_url, ...) 
#'     
#'     result <- result %>%
#'       cleanNULL()
#' 
#'   if (is.null(result$data[1][[1]])) {
#'     return(NULL)
#'   } else {
#'     output <- parse_site(result)
#'     return(output)
#'   }
#' }