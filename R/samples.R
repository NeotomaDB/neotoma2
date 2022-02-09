#' @title taxa
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @description Show the samples table
#' @param object Sites object to extract taxa table from
#' @export
setGeneric("taxa", function(object) {
  standardGeneric("taxa")
})

#' @export
setMethod(f = "samples",
          signature = "sites",
          definition = function(object) {
            
            counter = 0
            taxon_table <- c()
            length_object <- length(object)
            for (i in seq_len(length_object)) {
              length_collunits <- length(object[[i]]@collunits)
              siteid <- object[[i]]@siteid
              sitename <- object[[i]]@sitename
              #lat<-  mean(st_coordinates(object@geography)[, 2])
              #long <-   mean(st_coordinates(object@geography)[, 2])
              for (j in seq_len(length_collunits)){
                length_datasets <- length(object[[i]]@collunits[[j]]@datasets)
                
                for (k in seq_len(length_datasets)) {
                  length_samples <- length(object[[i]]@collunits[[j]]@datasets[[k]]@samples)
                  datasetid <- object[[i]]@collunits[[j]]@datasets[[k]]@datasetid
                  
                  #chron_default <- object[[i]]@collunits[[j]]@chronologies[[k]]@isdefault
                  #chron_name <- object[[i]]@collunits[[j]]@chronologies[[k]]@chronologyname
                  #agetype <- object[[i]]@collunits[[j]]@chronologies[[k]]@modelagetype
                  
                    for (l in seq_len(length_samples)) {
                      df <- object[[i]]$collunits[[j]]$datasets[[k]]$samples[[l]]$datum 
                      # Verify number of samples
                      counter = counter + 1
                      
                      df_sample <- df %>%
                        select(units, context, element, taxonid, symmetry, 
                               taxongroup, elementtype, variablename,ecologicalgroup)
                      
                      # Get ages
                      ageold <- object[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$ageolder
                      ageyoung <- object[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$ageyounger
                      agetype <- object[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$agetype
                      chronologyname <- object[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$chronologyname
                      chronologyid <- object[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$chronologyid
                        
                      df_sample <- df_sample %>%
                        mutate(siteid = siteid,
                               sitename = sitename,
                               datasetid = datasetid,
                               ageold = ageold,
                               ageyoung = ageyoung,
                               agetype = agetype,
                               chronologyname = chronologyname,
                               chronologyid = chronologyid
                               )
                      
                      df_sample <- df_sample %>%
                        select(siteid, sitename, datasetid, units, context, element, 
                               taxonid, symmetry, taxongroup, elementtype, variablename,
                               ecologicalgroup, ageold, ageyoung, agetype, chronologyname, chronologyid )
                      #   age_old,
                      # age_young, chron_default, chron_name, agetype
                      
                      taxon_table <- rbind(taxon_table, df_sample) %>%
                        distinct()
                    }
                  }
                }
              }
            return(taxon_table)
          }
          )