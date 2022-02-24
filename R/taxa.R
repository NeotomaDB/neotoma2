#' @import dplyr
#' @export

#' @title Extract taxonomic data from a set of sites.
#' @param object A \code{sites} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units, &cetera within the set of records.
setMethod(f = "taxa",
          signature = "sites",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              select(.data$units,
                     .data$context,
                     .data$element,
                     .data$taxonid,
                     .data$symmetry,
                     .data$taxongroup,
                     .data$elementtype,
                     .data$variablename,
                     .data$ecologicalgroup)
            return(tx_table)
          })

#' @title Extract taxonomic data from a single site.
#' @param object A \code{site} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units, &cetera within the set of records.
setMethod(f = "taxa",
          signature = "site",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              select(.data$units,
                     .data$context,
                     .data$element,
                     .data$taxonid,
                     .data$symmetry,
                     .data$taxongroup,
                     .data$elementtype,
                     .data$variablename,
                     .data$ecologicalgroup)
            return(tx_table)
          })
