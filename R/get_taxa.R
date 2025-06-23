#' @md
#' @title get_taxa
#' @description a sites object with the requested taxa.
#' @param x string taxa name or names
#' @param ... accepted arguments, see details for more information.
#' @returns A Neotoma2 sites object with datasets with the requested taxa.
#' 
#' @export
get_taxa <- function(x = NA, ...) {
    UseMethod("get_taxa", NA)
  }

#' @title Get Taxa Default
#' @param x Use a taxon ID to extract site information
#' @param ... accepted arguments, see details for more information.
#' @importFrom utils URLencode
#' @returns `sites` object containing the requested `taxa`
#' @export
get_taxa.default <- function(x, ...) {
  oo <- options(scipen = 9999999)
  on.exit(options(oo))
  
  taxa_data <- get_taxon(x, ...)
  taxa_names <- as.data.frame(x=taxa_data)$taxonname
  sites <- get_datasets(taxa=taxa_names)
  
  return(sites)
}
