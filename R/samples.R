utils::globalVariables(c("modelagetype", "isdefault"))

#' @title samples
#' @param x sites object
#' @description Obtain all samples within a sites object
#' @examples {
#' dw <- get_downloads(1)
#' pollen <- samples(dw)
#' }
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @returns `data.frame` with sample records
setMethod(f = "samples",
          signature = "sites",
          definition = function(x) {
            output <- purrr::map(x@sites, function(y) samples(y)) %>%
              dplyr::bind_rows()
            if(nrow(output) == 0){
              warnsite <- sprintf("No assigned samples. Did you run get_downloads()?")
              warning(warnsite)
            }
            return(output)
          }
)

#' @title samples
#' @param x site object
#' @description Obtain elements on the samples level
#' @examples \donttest{
#' marion <- get_sites(sitename = "Marion Lake") %>%
#'   get_datasets() %>%
#'   filter(datasettype == "pollen") %>%
#'   get_downloads()
#' pollen <- samples(marion)
#' }
#' @export
#' @returns `data.frame` with sample records
#' @import dplyr
setMethod(f = "samples",
          signature = "site",
          definition = function(x) {
            allids <- getids(x)
            siteinfo <- as.data.frame(x) %>%
              dplyr::left_join(allids, by = "siteid")
            sampset <- purrr::map(x@collunits@collunits,
                                  function(y) samples(y)) %>%
              dplyr::bind_rows() %>%
              dplyr::bind_rows() %>%
              dplyr::left_join(siteinfo, by = "datasetid") %>%
              dplyr::rename(sitenotes = notes)
            return(sampset)
          }
)

#' @title Get samples from a collectionunit or set of collection units:
#' @param x collunits object
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @returns `data.frame` with sample records
#' @description Obtain elements from collunits
setMethod(f = "samples",
          signature = "collunits",
          definition = function(x) {
            purrr::map(x@collunits, function(x) samples(x)) %>%
              dplyr::bind_rows()
          }
)

#' @title samples
#' @param x collunit object
#' @description Obtain elements from collunit
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate
#' @returns `data.frame` with sample records
#' @export
setMethod(f = "samples",
          signature = "collunit",
          definition = function(x) {
            precedence <- c("Calendar years BP",
                            "Calibrated radiocarbon years BP",
                            "Radiocarbon years BP", "Varve years BP")
            
            ids <- getids(x)
            # Check the chronologies to make sure everything is okay:
            if (length(chronologies(x)) > 0) {
              # This pulls the chronology IDs, then applies the Neotoma
              # age model precedence (see get_table('agetypes')).
              # It returns a value that is larger when your age reporting is
              # better.
              defaultchron <- purrr::map(chronologies(x)@chronologies,
                                         function(y) {
                                           data.frame(chronologyid = y@chronologyid,
                                                      isdefault = y@isdefault,
                                                      modelagetype = y@modelagetype,
                                                      chronologyname = y@chronologyname,
                                                      dateprepared = y@dateprepared)
                                         }) %>%
                dplyr::bind_rows() %>%
                dplyr::mutate(modelrank = match(modelagetype, rev(precedence)),
                              order = isdefault * match(modelagetype,
                                                        rev(precedence)))
              
              # Validation of default chrons, we want to check whether there
              # exists either multiple default chronologies for the same
              # time-frame or, alternately, no default chronology.
              all_na <- all(is.na(defaultchron$order))
              max_order <- max(defaultchron$order, na.rm = TRUE)
              if (sum(defaultchron$order == max_order, na.rm = TRUE) > 1) {
                if (any(is.na(defaultchron$dateprepared))) {
                  high_chron <- defaultchron$order == max_order
                  newmax_order <- which.max(defaultchron$chronologyid[high_chron])
                  defaultchron$order[high_chron][newmax_order] <- max_order + 1
                } else {
                  newmax_order <- which.max(defaultchron$dateprepared[defaultchron$order == max_order])
                  defaultchron$order[defaultchron$order == max_order][newmax_order] <- max_order + 1
                }
              }
              
              if (all_na == TRUE) {
                warnsite <- sprintf(
                  "The dataset %s has no default chronologies.",
                  ids$datasetid[1])
                warning(warnsite)
              } else if (sum(defaultchron$order == max_order,
                             na.rm = TRUE) > 1) {
                warnsite <- sprintf(
                  "The dataset %s has multiple default chronologies.
                   Chronology %s has been used.",
                  ids$datasetid[1],
                  defaultchron$chronologyid[which.max(defaultchron$order)])
                warning(warnsite)
                defaultchron <- defaultchron[which.max(defaultchron$order), ]
              } else {
                defaultchron <- defaultchron[which.max(defaultchron$order), ]
              }
            } else {
              defaultchron <- data.frame(chronologyid = NULL)
            }
            
            sampset <- purrr::map(datasets(x)@datasets,
                                  function(y) {
                                    dsid <- y$datasetid
                                    allsamp <- purrr::map(y@samples@samples,
                                                          function(z) {
                                                            whichage <- which(z@ages$chronologyid == defaultchron$chronologyid)
                                                            if (length(whichage) == 0) {
                                                              whichage <- 1
                                                            }
                                                            
                                                            if(dim(z@datum)[1] >0){
                                                              df <- data.frame(z@ages[whichage,],
                                                                         z@datum,
                                                                         analysisunitid = z@analysisunitid,
                                                                         sampleanalyst = toString(unique(unlist(z@sampleanalyst, use.names = FALSE))),
                                                                         sampleid = z@sampleid,
                                                                         depth = z@depth,
                                                                         thickness = z@thickness,
                                                                         samplename = z@samplename,
                                                                         row.names = NULL)
                                                            } else {
                                                              df <- data.frame()
                                                            }
                                                            return(df)
                                                            
                                                          }) %>%
                                      dplyr::bind_rows() %>%
                                      dplyr::mutate(datasetid = dsid)
                                    return(allsamp)
                                  }) %>%
              dplyr::bind_rows() %>%
              dplyr::left_join(as.data.frame(datasets(x)), by = "datasetid") %>%
              dplyr::rename(datasetnotes = notes)
            return(sampset)
          }
)