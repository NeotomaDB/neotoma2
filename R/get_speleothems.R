

speleo_helper <- function(sites) {
  ids <- getids(sites)
  cuids <- ids$collunitid
  
  #sites <- get_downloads(dsids)
  base_url <- paste0("data/speleothems/", cuids)
  result <- parseURL(base_url)
  
  if (length(result[2]$data) > 0) {
    speleo <- parse_speleothem(result)
  } else {
    speleo <- NULL
  }
  
  pared_ds <- purrr::map(sites@sites, function(x) {
    ycu <- purrr::map(x@collunits, function(z) {
      yds <- speleo[which(as.data.frame(speleo)$collectionunitid %in% z$collectionunitid)]
      z@speleothems <- yds
      return(z)
    })
    x@collunits@collunits <- ycu
    return(x)
  })
}

#' @title get_speleothems
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
#' speleo <- get_speleothems(c(2,5))
#' }
#' @export
get_speleothems.numeric <- function(x, ...) {
  if (length(x) > 0) {
    dsids <- paste0(x, collapse = ",")
  }
  st <- get_datasets(x)
  pared_ds <- speleo_helper(st)
  return(new("sites", sites = pared_ds))
}


#' @title Get Speleothem Data from a Sites object
#' @param x The numeric dataset ID from Neotoma
#' @param ... accepted arguments if numeric all_data
#' @returns The function returns either a single item of class `"try-error"`
#' describing the reason for failure (either misdefined parameters or an error
#' from the Neotoma API), or a sies object with speleothem data.
#' @examples {
#' ## Find speleothems by numeric datasetid:
#' speleo <- get_speleothems(c(2,5))
#' }
#' @export
get_speleothems.sites <- function(x, ...) {
  dsids <- getids(sites)$datasetids
  pared_ds <- speleo_helper(x, dsids)
  return(pared_ds)
}