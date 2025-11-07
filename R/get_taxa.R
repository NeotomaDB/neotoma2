#' @title get_taxa
#' @name get_taxa
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description a `sites` object with sites that contain the requested `taxa.`
#' @param x A `taxon` ID to extract site information
#' @param ... accepted arguments, see details for more information.
#' @details
#' The `get_taxa` function searches for a sites within Neotoma 
#' that contain the requested taxa.
#' The `get_taxa` command wraps the Neotoma API`.
#' The call itself accepts any one of the following
#' parameters:
#'  * `taxonid`  The unique taxon ID (integer) in Neotoma. Can be passed as a
#' vector of site IDs.
#' @returns A Neotoma2 sites object with datasets with the requested taxa.
#' @md
#' @export
get_taxa <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_taxa", x)
  } else {
    UseMethod("get_taxa", NA)
  }
}

#' @rdname get_taxa
#' @export
get_taxa.numeric <- function(x, ...) {
  oo <- options(scipen = 9999)
  on.exit(options(oo))
  taxa_data <- get_taxon(x, ...)
  taxa_names <- as.data.frame(taxa_data)$taxonname
  sites <- get_datasets(taxa = taxa_names)
  return(sites)
}

#' @rdname get_taxa
#' @export
get_taxa.default <- function(...) {
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  if ("all_data" %in% cl) {
    all_data <- cl$all_data
  } else {
    all_data <- TRUE
  }
  if ("taxa" %in% all(names(cl))){
    sites <- get_datasets(taxa = taxa, all_data = all_data)
  } else {
    stop("Please provide a taxon ID, taxon name, 
          or taxon object to extract site information.")
    return(NULL)
  }
  return(sites)
}