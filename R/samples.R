#' @title  samples
#' @param x sites object
#' @description Obtain elements on the samples level
#' @export
#' @import dplyr
setMethod(f = "samples",
          signature = "sites",
          definition = function(x) {
            
            counter = 0
            taxon_table <- c()
            length_object <- length(x)
            
            allids <- getids(x)
            siteinfo <- as.data.frame(x) %>%
              left_join(allids, by = "siteid") %>%
              left_join(as.data.frame(datasets(x)), by = "datasetid") %>%
              rename(sitenotes = notes.x, datasetnotes = notes.y)
            
            sampset <- purrr::map(datasets(x)@datasets,
                                  function(y) {
                                    dsid <- y$datasetid
                                    allsamp <- purrr::map(y$samples@samples,
                                                          function(z) {
                                                            data.frame(z@ages,
                                                                       z@datum,
                                                                       depth = z@depth,
                                                                       thickness = z@thickness,
                                                                       samplename = z@samplename)
                                                          }) %>%
                                      bind_rows() %>%
                                      mutate(datasetid = dsid)
                                  }) %>%
              bind_rows() %>%
              left_join(siteinfo, by = "datasetid")
            return(sampset)
          }
)

#' @title  samples
#' @param x site object
#' @description Obtain elements on the samples level
#' @export
#' @import dplyr
setMethod(f = "samples",
          signature = "site",
          definition = function(x) {
            
            counter = 0
            taxon_table <- c()
            length_object <- length(x)
            
            allids <- getids(x)
            siteinfo <- as.data.frame(x) %>%
              left_join(allids, by = "siteid") %>%
              left_join(as.data.frame(datasets(x)), by = "datasetid") %>%
              rename(sitenotes = notes.x, datasetnotes = notes.y)
            
            sampset <- purrr::map(datasets(x)@datasets,
                                  function(y) {
                                    dsid <- y$datasetid
                                    allsamp <- purrr::map(y$samples@samples,
                                                          function(z) {
                                                            data.frame(z@ages,
                                                                       z@datum,
                                                                       depth = z@depth,
                                                                       thickness = z@thickness,
                                                                       samplename = z@samplename)
                                                          }) %>%
                                      bind_rows() %>%
                                      mutate(datasetid = dsid)
                                  }) %>%
              bind_rows() %>%
              left_join(siteinfo, by = "datasetid")
            return(sampset)
          }
)
