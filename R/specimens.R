#' @title specimens
#' @author Socorro Dominguez \email{s.dominguez@@ht-data.com}
#' @import dplyr
#' @description
#' Information table for Specimens
#' @param x Use a sites element that has specimens added.
#' @return The function returns a specimens summary table.
#' @examples \dontrun{
#' # To return a specimens table do:
#' my_specimens <- get_specimens(19832)
#' my_tbl <- specimens(my_specimens)
#' }
#' @export
setMethod(f = "specimens",
          signature = "sites",
          definition = function(x) {
            
            output <- purrr::map(x@sites, function(y) specimens(y)) %>%
              dplyr::bind_rows()
            if(nrow(output) == 0){
            warnsite <- sprintf("No assigned specimens. Did you run get_specimens()")
            warning(warnsite)
            }
            
            return(output)
          }
)

#' @title s
#' @param x site object
#' @description Obtain elements on the specimens level
#' @export
#' @import dplyr
setMethod(f = "specimens",
          signature = "site",
          definition = function(x) {
            
            #allids <<- getids(x)
            assign("allids", getids(x))
            siteinfo <- as.data.frame(x) %>%
              dplyr::left_join(allids, by = "siteid") %>%
              dplyr::left_join(as.data.frame(datasets(x)), by = "datasetid") %>%
              dplyr::rename(sitenotes = notes.x,
                            datasetnotes = notes.y)
            
            sampset <- purrr::map(x@collunits@collunits,
                                  function(y) specimens(y)) %>%
              dplyr::bind_rows() %>%
              dplyr::bind_rows() %>%
              dplyr::left_join(siteinfo, by = "datasetid")
            
            return(sampset)
          }
)

### Collunits

#' @title specimens
#' @param x collunits object
#' @description Obtain elements from collunits
setMethod(f = "specimens",
          signature = "collunits",
          definition = function(x) {
            allids <- get("allids", parent.frame())
            output <- purrr::map(x@collunits, function(x) specimens(x)) %>%
              dplyr::bind_rows()
            return(output)
          }
)

#' @title specimens
#' @param x collunit object
#' @description Obtain elements from collunit
setMethod(f = "specimens",
          signature = "collunit",
          definition = function(x) {
            allids <- get("allids", parent.frame())
            precedence <- c("Calendar years BP",
                            "Calibrated radiocarbon years BP",
                            "Radiocarbon years BP", "Varve years BP")
            sampleset <- samples(x)
            
            if(nrow(sampleset) == 0){
              stop("Not enough data. Have you run get_specimens()?")
            }
            sampleset %>%
              dplyr::select('datasetid', 'sampleid', 'taxonid', 'age',
                            'agetype', 'ageolder', 'ageyounger', 'chronologyid',
                            'chronologyname', 'units', 'value', 'context', 'element',
                            'taxongroup', 'variablename', 'ecologicalgroup', 'analysisunitid', 
                            'sampleanalyst', 'depth', 'thickness', 'samplename')
            
            sampset <- purrr::map(datasets(x)@datasets,
                                  function(y) {
                                    if(length(y@specimens@specimens) != 0){
                                      dsid <- as.character(y$datasetid)
                                      allspec <- purrr::map(y@specimens@specimens,
                                                            function(z) {
                                                              data.frame(
                                                                sampleid = z@sampleid,
                                                                taxonid = z@taxonid,
                                                                specimendid = z@specimenid,
                                                                taxonname = z@taxonname,
                                                                portion = z@portion,
                                                                sex = z@sex,
                                                                domesticstatus = z@domesticstatus,
                                                                taphonomictype = z@taphonomictype,
                                                                elementtype = z@elementtype,
                                                                symmetry = z@symmetry,
                                                                row.names = NULL)
                                                            }) %>%
                                        dplyr::bind_rows() %>%
                                        dplyr::mutate(datasetid = dsid)
                                    } else {
                                      allspec <- data.frame()
                                      warnsite <- sprintf("The dataset %s has no assigned specimens.", as.character(y$datasetid))
                                      warning(warnsite)
                                      return(allspec)
                                    }
                                  }) %>%
              dplyr::bind_rows()
            
            if(nrow(sampset) != 0){
              new_sampset <- left_join(sampset, sampleset, by = c('datasetid', 'sampleid', 'taxonid'))
            } else {
              
              new_sampset <- data.frame()
            }
            return(new_sampset)
          }
)