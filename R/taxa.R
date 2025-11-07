utils::globalVariables(c("context", "sites"))
#' @title Extract taxonomic data from a set of sites.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom dplyr bind_rows distinct group_by summarise
#' @importFrom purrr map
#' @param object A \code{sites} object.
#' @returns A \code{data.frame} reporting the taxa/data objects, units,
#' elements and other features within a set of records.
#' @description Extract taxonomic data from a set of sites.
#' @examples \donttest{
#' somesites <- get_sites(datasettype = "diatom") %>%
#'   get_downloads()
#' diatomtaxa <- taxa(somesites)
#'   }
#' @md
#' @export
setMethod(f = "taxa",
  signature = "sites",
  definition = function(object) {
    output <- map(object@sites, function(y) taxa(y)) %>%
      bind_rows() %>%
      group_by(units,
               context,
               element,
               taxonid,
               symmetry,
               taxongroup,
               elementtype,
               variablename,
               ecologicalgroup) %>%
      summarise(samples = sum(samples),
                sites = sum(sites), .groups = "keep")
    if (nrow(output) == 0) {
      warnsite <- sprintf("No assigned samples. Did you run get_downloads()?")
      warning(warnsite)
    }
    return(output)
  }
)

#' @rdname taxa
#' @export
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

#' @rdname taxa
#' @export
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

#' @rdname taxa
#' @export
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