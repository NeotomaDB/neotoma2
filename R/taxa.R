#' @title taxa
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @description Show the taxa table
#' @param object Sites object to extract taxa table from
#' @export
setGeneric("taxa", function(object) {
  standardGeneric("taxa")
})

#' @export
setMethod(f = "taxa",
          signature = "sites",
          definition = function(object) {
            
            counter = 0
            taxon_table <- c()
            length_object <- length(object)
            for (i in seq_len(length_object)) {
              length_collunits <- length(object[[i]]@collunits)
              for (j in seq_len(length_collunits)){
                length_datasets <- length(object[[i]]@collunits[[j]]@datasets)
                for (k in seq_len(length_datasets)) {
                  length_samples <- length(object[[i]]@collunits[[j]]@datasets[[k]]@samples)
                    for (l in seq_len(length_samples)) {
                      df <- object[[i]]$collunits[[j]]$datasets[[k]]$samples[[l]]$datum 
                      # Verify number of samples
                      counter = counter + 1
                      
                      df_sample <- df %>%
                        select(units, context, element, taxonid, symmetry, 
                               taxongroup, elementtype, variablename,ecologicalgroup)
                      
                      taxon_table <- rbind(taxon_table, df_sample) %>%
                        distinct()
                    }
                  }
                }
              }
            return(taxon_table)
          }
          )