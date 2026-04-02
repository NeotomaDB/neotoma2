#' @title samples
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @param x sites object
#' @description Obtain all samples within a sites object
#' @examples \dontrun{
#' # Get full data download from API and create a long table with samples data.
#' dw <- get_downloads(1)
#' pollen <- samples(dw)
#' }
#' @importFrom dplyr bind_rows left_join rename mutate
#' @importFrom purrr map
#' @returns `data.frame` with sample records
#' @md
#' @export
setMethod(f = "samples",
          signature = "sites",
          definition = function(object,chronname = NULL) {
            output <- map(object@sites, function(y) samples(y, chronname = chronname)) %>%
              bind_rows() # %>%
            # Handle NAs to allow distinct to work properly
            #distinct(.data$sampleid, .keep_all = TRUE)
            if (nrow(output) == 0) {
              warnsite <- sprintf("No assigned samples. Did you run get_downloads()?")
              warning(warnsite)
            }
            return(output)
          }
)

#' @rdname samples
#' @export
setMethod(f = "samples",
          signature = "site",
          definition = function(object,chronname = NULL) {
            allids <- getids(object)
            siteinfo <- as.data.frame(object) %>%
              left_join(allids, by = "siteid")
            sampset <- map(object@collunits@collunits,
                           function(y) samples(y, chronname = chronname)) %>%
              bind_rows() %>%
              bind_rows() %>%
              left_join(siteinfo, by = "datasetid") %>%
              rename(sitenotes = .data$notes)
            return(sampset)
          }
)

#' @rdname samples
#' @export
setMethod(f = "samples",
          signature = "collunits",
          definition = function(object, chronname = NULL) {
            map(object@collunits, function(object) samples(object, chronname=chronname)) %>%
              bind_rows()
          }
)

#' @rdname samples
#' @export
#' 
setMethod(f = "samples",
          signature = "collunit",
          definition = function(object, chronname) {
            chron_exists <- FALSE
            precedence <- c("Calendar years BP",
                            "Calibrated radiocarbon years BP",
                            "Radiocarbon years BP", "Varve years BP")
            ids <- getids(object)
            # Check the chronologies to make sure everything is okay:
            if (length(chronologies(object)) > 0) {
              # This pulls the chronology IDs, then applies the Neotoma
              # age model precedence (see get_table('agetypes')).
              # It returns a value that is larger when your age reporting is
              # better.
              defaultchron <- map(chronologies(object)@chronologies,
                                  function(y) {
                                    data.frame(chronologyid = y@chronologyid,
                                               isdefault = y@isdefault,
                                               modelagetype = y@modelagetype,
                                               chronologyname = y@chronologyname,
                                               dateprepared = y@dateprepared)
                                  }) %>%
                bind_rows() %>%
                mutate(modelrank = match(.data$modelagetype, rev(precedence)),
                       order = .data$isdefault * match(.data$modelagetype,
                                                       rev(precedence)))
              #print(defaultchron)
              #print(unique(defaultchron$modelagetype))
              #print(unique(defaultchron$isdefault))
              # Validation of default chrons, we want to check whether there
              # exists either multiple default chronologies for the same
              # time-frame or, alternately, no default chronology.
              all_na <- all(is.na(defaultchron$order))
              #print(all_na)
              #print(defaultchron$order)
              max_order <- max(defaultchron$order, na.rm = TRUE)
              if (sum(defaultchron$order == max_order, na.rm = TRUE) > 1) {
                if (any(is.na(defaultchron$dateprepared))) {
                  high_chron <- defaultchron$order == max_order
                  newmax_order <- which.max(defaultchron$chronologyid[high_chron])
                  defaultchron$order[high_chron][newmax_order] <- max_order + 1
                } else {
                  newmax_order <- which.max(defaultchron$dateprepared[
                    defaultchron$order == max_order])
                  defaultchron$order[defaultchron$order == max_order][
                    newmax_order] <- max_order + 1
                }
              }
              if (all_na == TRUE) {
                warnsite <- sprintf("The dataset %s has no default chronologies.",
                                    ids$datasetid[1])
                warning(warnsite)
                
              } 
              
              chron_exists <- !is.null(chronname) &&
                chronname %in% defaultchron$chronologyname
              
              if (all_na == TRUE) {
                
                warnsite <- sprintf("The dataset %s has no default chronologies.",
                                    ids$datasetid[1])
                warning(warnsite)
                
              } else if (chron_exists) {
                
                # chronology explicitly requested and exists
                matches <- defaultchron$chronologyname == chronname
                
                if (sum(matches, na.rm = TRUE) == 1) {
                  defaultchron <- defaultchron[matches, ]
                }
                
                if (sum(matches, na.rm = TRUE) > 1) {
                  defaultchron <- defaultchron[matches, ][1,]
                  warnsite <- sprintf(
                    "The dataset %s has multiple chronologies matching chronname %s. Chronology %s has been used.",
                    ids$datasetid[1], chronname, defaultchron$chronologyid)
                  warning(warnsite)
                }
                
              } else {
                
                # either no chronname OR chronname invalid
                defaultchron <- defaultchron[which.max(defaultchron$order), ]
                
                if (!is.null(chronname)) {
                  warnsite <- sprintf(
                    "The dataset %s has no chronology matching chronname %s. Default chronology %s has been used.",
                    ids$datasetid[1], chronname, defaultchron$chronologyid)
                  warning(warnsite)
                }
                
              } 
            } else {
              defaultchron <- data.frame(chronologyid = NULL)
            }
            #print(paste0("default chron: ", defaultchron))
            sampset <- map(datasets(object)@datasets,
                           function(y) {
                             dsid <- y$datasetid
                             allsamp <-
                               map(y@samples@samples,
                                   function(z) {
                                     whichage <-
                                       which(z@ages$chronologyid ==
                                               defaultchron$chronologyid)
                                     if (length(whichage) == 0) {
                                       if (chron_exists) {
                                         whichage <- NA
                                       } else {
                                         whichage <- 1
                                       }
                                     }
                                     if (dim(z@datum)[1] > 0) {
                                       df <-
                                         data.frame(z@ages[whichage,],
                                                    z@datum,
                                                    analysisunitid = z@analysisunitid,
                                                    sampleanalyst =
                                                      toString(unique(unlist(
                                                        z@sampleanalyst,
                                                        use.names = FALSE))),
                                                    sampleid = z@sampleid,
                                                    depth = z@depth,
                                                    thickness = z@thickness,
                                                    samplename = z@samplename,
                                                    row.names = NULL)
                                       #print(z$ages)
                                     } else {
                                       df <- data.frame()
                                     }
                                     return(df)
                                   }) %>%
                               bind_rows() %>%
                               mutate(datasetid = dsid)
                             return(allsamp)
                           }) %>%
              bind_rows() %>%
              left_join(as.data.frame(datasets(object)), by = "datasetid") %>%
              rename(datasetnotes = .data$notes)
            if (!is.null(chronname) && sum(defaultchron$chronologyname == chronname, na.rm = TRUE) != 0) {
              sampset = sampset %>% dplyr::filter(!is.na(chronologyid))}
            return(sampset)
          }
)