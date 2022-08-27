utils::globalVariables(c("context"))

#' @import dplyr
#' @export

#' @title Extract taxonomic data from a set of sites.
#' @description From a sites object,
#' @param object A \code{sites} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units, &cetera within the set of records.
setMethod(f = "taxa",
          signature = "sites",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              group_by(.data$units, .data$context, .data$element, .data$taxonid, 
                       .data$symmetry,.data$taxongroup, .data$elementtype, 
                       .data$variablename,.data$ecologicalgroup, .data$siteid) %>%
              summarise(samples = n(), .groups = "keep") %>%
              group_by(.data$units, .data$context, .data$element, .data$taxonid, 
                       .data$symmetry,.data$taxongroup, .data$elementtype, 
                       .data$variablename, .data$ecologicalgroup) %>%
              summarise(sites = n(), samples = sum(samples), .groups = "keep")
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
              group_by(.data$units, .data$context,.data$element, 
                       .data$taxonid, .data$symmetry,.data$taxongroup, 
                       .data$elementtype, .data$variablename,
                       .data$ecologicalgroup, .data$siteid) %>%
              summarise(samples = n(), .groups = "keep") %>%
              group_by(.data$units, .data$context, .data$element, 
                       .data$taxonid, .data$symmetry, .data$taxongroup, 
                       .data$elementtype, .data$variablename, .data$ecologicalgroup) %>%
              summarise(sites = n(), samples = sum(samples), .groups = "keep")
            return(tx_table)
          })

#' @title Extract taxonomic data from a set of sites.
#' @param object A \code{collunits} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units, &cetera within the set of records.
setMethod(f = "taxa",
          signature = "collunits",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              group_by(.data$units, .data$context,.data$element, 
                       .data$taxonid, .data$symmetry,.data$taxongroup, 
                       .data$elementtype, .data$variablename,
                       .data$ecologicalgroup, .data$siteid) %>%
              summarise(samples = n(), .groups = "keep") %>%
              group_by(.data$units, .data$context, .data$element, 
                       .data$taxonid, .data$symmetry, .data$taxongroup, 
                       .data$elementtype, .data$variablename, .data$ecologicalgroup) %>%
              summarise(sites = n(), samples = sum(samples), .groups = "keep")
            return(tx_table)
          })

#' @title Extract taxonomic data from a set of sites.
#' @param object A \code{collunit} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units, &cetera within the set of records.
setMethod(f = "taxa",
          signature = "collunit",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              group_by(.data$units, .data$context,.data$element, 
                       .data$taxonid, .data$symmetry,.data$taxongroup, 
                       .data$elementtype, .data$variablename,
                       .data$ecologicalgroup, .data$siteid) %>%
              summarise(samples = n(), .groups = "keep") %>%
              group_by(.data$units, .data$context, .data$element, 
                       .data$taxonid, .data$symmetry, .data$taxongroup, 
                       .data$elementtype, .data$variablename, .data$ecologicalgroup) %>%
              summarise(sites = n(), samples = sum(samples), .groups = "keep")
            return(tx_table)
          })

