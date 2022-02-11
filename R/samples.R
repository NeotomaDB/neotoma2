#' @export
#' @import dplyr
setMethod(f = "samples",
          signature = "sites",
          definition = function(x) {
            
            counter = 0
            taxon_table <- c()
            length_object <- length(x)
            for (i in seq_len(length_object)) {
              length_collunits <- length(x[[i]]@collunits)
              siteid <- x[[i]]@siteid
              sitename <- x[[i]]@sitename
              #lat<-  mean(st_coordinates(x@geography)[, 2])
              #long <-   mean(st_coordinates(x@geography)[, 2])
              for (j in seq_len(length_collunits)){
                length_datasets <- length(x[[i]]@collunits[[j]]@datasets)
                
                for (k in seq_len(length_datasets)) {
                  length_samples <- length(x[[i]]@collunits[[j]]@datasets[[k]]@samples)
                  datasetid <- x[[i]]@collunits[[j]]@datasets[[k]]@datasetid
                  
                  #chron_default <- x[[i]]@collunits[[j]]@chronologies[[k]]@isdefault
                  #chron_name <- x[[i]]@collunits[[j]]@chronologies[[k]]@chronologyname
                  #agetype <- x[[i]]@collunits[[j]]@chronologies[[k]]@modelagetype
                  
                  for (l in seq_len(length_samples)) {
                    df <- x[[i]]$collunits[[j]]$datasets[[k]]$samples[[l]]$datum
                    # Verify number of samples
                    counter = counter + 1
                    
                    df_sample <- df %>%
                      select(units, context, element, taxonid, symmetry,
                             taxongroup, elementtype, variablename,ecologicalgroup)
                    
                    default_chron <- x[[i]]@collunits[[j]]@defaultchronology
                    # Filter ages dataframe
                    df <- x[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages
                    df <- df %>% dplyr::filter(chronologyid == default_chron)
                    
                    # Ages
                    ageold <- df$ageolder
                    ageyoung <- df$ageyounger
                    agetype <- df$agetype
                    chronologyname <- df$chronologyname
                    chronologyid <- df$chronologyid
                    
                    if (nrow(df) == 0) {
                      ageold <- NA_integer_
                      ageyoung <- NA_integer_
                      agetype <- NA_character_
                      chronologyname <- NA_character_
                      chronologyid <- NA_integer_
                    }
                    
                    
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
                             ecologicalgroup, agetype, ageold, ageyoung, chronologyname, 
                             chronologyid)
                    
                    taxon_table <- rbind(taxon_table, df_sample) %>%
                      distinct()
                  }
                }
              }
            }
            return(taxon_table)
          }
)
