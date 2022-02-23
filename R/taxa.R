#' @import dplyr
#' @export

setMethod(f = "taxa",
          signature = "sites",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              select(units, context, element, taxonid, symmetry, taxongroup, 
                     elementtype, variablename, ecologicalgroup)
            return(tx_table)
          })

setMethod(f = "taxa",
          signature = "site",
          definition = function(object) {
            samples <- samples(object)
            tx_table <- samples %>%
              select(units, context, element, taxonid, symmetry, taxongroup, 
                     elementtype, variablename, ecologicalgroup)
            return(tx_table)
          })