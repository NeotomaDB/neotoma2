#' @export
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

                      # Get ages
                      ageold <- x[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$ageolder
                      ageyoung <- x[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$ageyounger
                      agetype <- x[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$agetype
                      chronologyname <- x[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$chronologyname
                      chronologyid <- x[[i]]@collunits[[j]]@datasets[[k]]@samples[[l]]@ages$chronologyid

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
