utils::globalVariables(c("context"))

#' @import dplyr
#' @export

#' @title Extract taxonomic data from a set of sites.
#' @description From a sites object,
#' @param object A \code{sites} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units,
#' elements and other features within a set of records.
#' @examples
#' somesites <- get_sites(datasettype = "diatom", limit = 3) %>%
#'   get_downloads()
#' diatomtaxa <- taxa(somesites)
#' common_taxa <- diatomtaxa %>%
#'   dplyr::filter(sites == 3)
#'
setMethod(f = "taxa",
          signature = "sites",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              group_by(units,
                context,
                element,
                taxonid,
                symmetry,
                taxongroup,
                elementtype,
                variablename,
                ecologicalgroup,
                siteid) %>%
              summarise(samples = n(), .groups = "keep") %>%
              group_by(units,
                context,
                element,
                taxonid,
                symmetry,
                taxongroup,
                elementtype,
                variablename,
                ecologicalgroup) %>%
              summarise(sites = n(), samples = sum(samples), .groups = "keep")
            return(tx_table)
          })

#' @title Extract taxonomic data from a single site.
#' @param object A \code{site} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units,
#' elements and other features within a set of records.
#' @examples
#' somesites <- get_sites(datasettype = "pollen", limit = 3) %>%
#'   get_downloads()
#' diatomtaxa <- taxa(somesites[[1]])
setMethod(f = "taxa",
          signature = "site",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              group_by(units,
                context,
                element,
                taxonid,
                symmetry,
                taxongroup,
                elementtype,
                variablename,
                ecologicalgroup,
                siteid) %>%
              summarise(samples = n(), .groups = "keep") %>%
              group_by(units,
                context,
                element,
                taxonid,
                symmetry,
                taxongroup,
                elementtype,
                variablename,
                ecologicalgroup) %>%
              summarise(sites = n(), samples = sum(samples), .groups = "keep")
            return(tx_table)
          })

#' @title Extract taxonomic data from a set of sites.
#' @param object A \code{collunits} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units,
#' elements and other features within a set of records.
setMethod(f = "taxa",
          signature = "collunits",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              group_by(units,
                context,
                element,
                taxonid,
                symmetry,
                taxongroup,
                elementtype,
                variablename,
                ecologicalgroup,
                siteid) %>%
              summarise(samples = n(), .groups = "keep") %>%
              group_by(units,
                context,
                element,
                taxonid,
                symmetry,
                taxongroup,
                elementtype,
                variablename,
                ecologicalgroup) %>%
              summarise(sites = n(), samples = sum(samples), .groups = "keep")
            return(tx_table)
          })

#' @title Extract taxonomic data from a set of sites.
#' @param object A \code{collunit} object.
#' @return A \code{data.frame} reporting the taxa/data objects, units,
#' elements and other features within a set of records.
setMethod(f = "taxa",
          signature = "collunit",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              group_by(units,
                context,
                element,
                taxonid,
                symmetry,
                taxongroup,
                elementtype,
                variablename,
                ecologicalgroup,
                siteid) %>%
              summarise(samples = n(), .groups = "keep") %>%
              group_by(units,
                context,
                element,
                taxonid,
                symmetry,
                taxongroup,
                elementtype,
                variablename,
                ecologicalgroup) %>%
              summarise(sites = n(), samples = sum(samples), .groups = "keep")
            return(tx_table)
          })
