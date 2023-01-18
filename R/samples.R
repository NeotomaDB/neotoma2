utils::globalVariables(c("modelagetype", "isdefault", "allids", "<<-"))

#' @title samples
#' @param x sites object
#' @description Obtain elements on the samples level
#' @export
#' @import dplyr
setMethod(f = "samples",
          signature = "sites",
          definition = function(x) {
            output <- purrr::map(x@sites, function(y) samples(y)) %>%
              dplyr::bind_rows()
            return(output)
          }
)

#' @title samples
#' @param x site object
#' @description Obtain elements on the samples level
#' @export
#' @import dplyr
setMethod(f = "samples",
          signature = "site",
          definition = function(x) {
            allids <<- getids(x)
            siteinfo <- as.data.frame(x) %>%
              dplyr::left_join(allids, by = "siteid") %>%
              dplyr::left_join(as.data.frame(datasets(x)), by = "datasetid") %>%
              dplyr::rename(sitenotes = notes.x,
                            datasetnotes = notes.y)
            sampset <- purrr::map(x@collunits@collunits,
              function(y) samples(y)) %>%
                dplyr::bind_rows() %>%
                dplyr::bind_rows() %>%
                dplyr::left_join(siteinfo, by = "datasetid")

            return(sampset)
          }
)

#' @title samples
#' @param x collunits object
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
setMethod(f = "samples",
          signature = "collunit",
          definition = function(x) {
            precedence <- c("Calendar years BP",
                            "Calibrated radiocarbon years BP",
                            "Radiocarbon years BP", "Varve years BP")

            # Check the chronologies to make sure everything is okay:
            if (length(chronologies(x)) > 0) {
              # This pulls the chronology IDs, then applies the Neotoma
              # age model precedence (see get_table('agetypes')).
              # It returns a value that is larger when your age reporting is
              # better.
              defaultchron <- purrr::map(chronologies(x)@chronologies,
                function(y) {
                  data.frame(chronologyid = as.character(y@chronologyid),
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
                  newmax_order <- which.max(defaultchron$chronologyid[defaultchron$order == max_order])
                  defaultchron$order[defaultchron$order == max_order][newmax_order] <- max_order + 1
                } else {
                  newmax_order <- which.max(defaultchron$dateprepared[defaultchron$order == max_order])
                  defaultchron$order[defaultchron$order == max_order][newmax_order] <- max_order + 1
                }
              }

              if (all_na == TRUE) {
                warnsite <- sprintf(
                  "The dataset %d has no default chronologies.",
                  allids$datasetid[1])
                warning(warnsite)
              } else if (sum(defaultchron$order == max_order, na.rm = TRUE) > 1) {
                warnsite <- sprintf(
                  "The dataset %d has multiple default chronologies.
                   Chronology %d has been used.",
                   allids$datasetid[1],
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
                                                            data.frame(z@ages[whichage,],
                                                                       z@datum,
                                                                       analysisunitid = z@analysisunitid,
                                                                       sampleanalyst = toString(unique(unlist(z@sampleanalyst, use.names = FALSE))),
                                                                       sampleid = z@sampleid,
                                                                       depth = z@depth,
                                                                       thickness = z@thickness,
                                                                       samplename = z@samplename,
                                                                       row.names = NULL)
                                                          }) %>%
                                      dplyr::bind_rows() %>%
                                      dplyr::mutate(datasetid = as.character(dsid))
                                  }) %>%
              dplyr::bind_rows()

            return(sampset)
          }
)
