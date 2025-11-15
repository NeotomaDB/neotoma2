#' @title samples
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @param x sites object
#' @description Obtain all samples within a sites object
#' @examples {
#' tryCatch({
#' dw <- get_downloads(1)
#' pollen <- samples(dw)
#' }, error = function(e) {
#'  message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @importFrom dplyr bind_rows left_join rename mutate
#' @importFrom purrr map
#' @returns `data.frame` with sample records
#' @md
#' @export
setMethod(f = "samples",
  signature = "sites",
  definition = function(x) {
    output <- map(x@sites, function(y) samples(y)) %>%
      bind_rows()  %>%
      # Handle NAs to allow distinct to work properly
      distinct(.data$sampleid, .keep_all = TRUE)
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
  definition = function(x) {
    allids <- getids(x)
    siteinfo <- as.data.frame(x) %>%
      left_join(allids, by = "siteid")
    sampset <- map(x@collunits@collunits,
                   function(y) samples(y)) %>%
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
  definition = function(x) {
    map(x@collunits, function(x) samples(x)) %>%
      bind_rows()
  }
)

#' @rdname samples
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
      defaultchron <- map(chronologies(x)@chronologies,
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
      } else if (sum(defaultchron$order == max_order, na.rm = TRUE) > 1) {
        warnsite <- sprintf("The dataset %s has multiple default chronologies.
                             Chronology %s has been used.", ids$datasetid[1],
                            defaultchron$chronologyid[
                              which.max(defaultchron$order)])
        warning(warnsite)
        defaultchron <- defaultchron[which.max(defaultchron$order), ]
      } else {
        defaultchron <- defaultchron[which.max(defaultchron$order), ]
      }
    } else {
      defaultchron <- data.frame(chronologyid = NULL)
    }
    sampset <- map(datasets(x)@datasets,
                   function(y) {
                     dsid <- y$datasetid
                     allsamp <-
                       map(y@samples@samples,
                           function(z) {
                             whichage <-
                               which(z@ages$chronologyid ==
                                     defaultchron$chronologyid)
                             if (length(whichage) == 0) {
                               whichage <- 1
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
      left_join(as.data.frame(datasets(x)), by = "datasetid") %>%
      rename(datasetnotes = .data$notes)
    return(sampset)
  }
)