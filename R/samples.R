#' @title samples
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @param x sites object
#' @param chronname Optional character name of a chronology to report ages
#'   against. When `NULL` (default) the Neotoma default-chronology precedence
#'   is applied. If the named chronology is not present for a collection unit,
#'   the default is used for that unit, and a single warning lists the
#'   affected datasets.
#' @description Obtain all samples within a sites object. When `chronname` is
#'   given the result has a logical column `chronname_matched`: `TRUE` when the
#'   sample's age comes from the requested chronology, `FALSE` when the
#'   collection unit does not have it and the default chronology was used.
#' @examples \dontrun{
#' # Get full data download from API and create a long table with samples data.
#' dw <- get_downloads(1)
#' pollen <- samples(dw)
#' }
#' @importFrom dplyr bind_rows left_join rename mutate
#' @importFrom purrr map
#' @returns `data.frame` with sample records (plus `chronname_matched` when
#'   `chronname` is supplied).
#' @md
#' @export
setMethod(
  f = "samples",
  signature = "sites",
  definition = function(x, chronname = NULL) {
    output <- map(x@sites, function(y) samples_site(y, chronname)) %>%
      bind_rows()
    warn_chronname_fallback(output, chronname, getids(x))
    if (nrow(output) == 0) {
      warnsite <- sprintf("No assigned samples. Did you run get_downloads()?")
      warning(warnsite)
    }
    return(output)
  }
)

#' @rdname samples
#' @export
setMethod(
  f = "samples",
  signature = "site",
  definition = function(x, chronname = NULL) {
    output <- samples_site(x, chronname)
    warn_chronname_fallback(output, chronname, getids(x))
    output
  }
)

#' @rdname samples
#' @export
setMethod(
  f = "samples",
  signature = "collunits",
  definition = function(x, chronname = NULL) {
    output <- map(x@collunits, function(y) samples_collunit(y, chronname)) %>%
      bind_rows()
    warn_chronname_fallback(output, chronname, getids(x))
    output
  }
)

#' @rdname samples
#' @export
setMethod(
  f = "samples",
  signature = "collunit",
  definition = function(x, chronname = NULL) {
    output <- samples_collunit(x, chronname)
    warn_chronname_fallback(output, chronname, getids(x))
    output
  }
)

#' @title Samples of one `site`, without the chronology fallback warning.
#' @description The public `samples()` methods call this and warn once at the
#' end, so a `sites` object with many collection units gives one warning rather
#' than one per unit.
#' @param x A `site` object.
#' @param chronname Optional chronology name.
#' @returns `data.frame` of samples.
#' @noRd
samples_site <- function(x, chronname = NULL) {
  allids <- getids(x)
  siteinfo <- as.data.frame(x) %>%
    left_join(allids, by = "siteid")
  map(
    x@collunits@collunits,
    function(y) samples_collunit(y, chronname)
  ) %>%
    bind_rows() %>%
    left_join(siteinfo, by = "datasetid") %>%
    rename(sitenotes = "notes")
}

#' @title One warning for datasets that lack the requested chronology.
#' @description `samples(chronname = )` falls back to the default chronology
#' for a collection unit that does not have the named one; those rows have
#' `chronname_matched == FALSE`. Report them in a single, single-line warning.
#' @param out `data.frame` returned by `samples_collunit()` (or bound together).
#' @param chronname The requested chronology name (or `NULL`).
#' @param ids `data.frame` from `getids()` mapping datasets to collection units.
#' @returns `NULL`, invisibly.
#' @noRd
warn_chronname_fallback <- function(out, chronname, ids) {
  if (is.null(chronname) || !"chronname_matched" %in% names(out)) {
    return(invisible(NULL))
  }
  bad <- unique(out$datasetid[which(!out$chronname_matched)])
  if (length(bad) == 0) {
    return(invisible(NULL))
  }
  collunits <- unique(ids$collunitid[ids$datasetid %in% bad])
  shorten <- function(v) {
    if (length(v) > 10) {
      paste0(paste(v[1:10], collapse = ", "), ", and ", length(v) - 10, " more")
    } else {
      paste(v, collapse = ", ")
    }
  }
  warning(
    sprintf(
      paste0(
        "Chronology '%s' was not found for collection unit(s) %s ",
        "(dataset(s) %s). The default chronology was used for them; ",
        "see the `chronname_matched` column (FALSE)."
      ),
      chronname, shorten(collunits), shorten(bad)
    ),
    call. = FALSE
  )
  invisible(NULL)
}

#' @title Samples of one `collunit`, without the chronology fallback warning.
#' @param x A `collunit` object.
#' @param chronname Optional chronology name.
#' @returns `data.frame` of samples, with `chronname_matched` when `chronname`
#'   is given.
#' @noRd
samples_collunit <- function(x, chronname = NULL) {
  # A collection unit with no datasets (e.g. its datasets were deleted
  # upstream) contributes no samples. Return an empty data.frame so the
  # bind_rows() in the site/sites/collunits methods simply skips it instead
  # of erroring on an empty/NULL datasets slot.
  if (length(datasets(x)) == 0) {
    return(data.frame())
  }
  chron_exists <- FALSE
  precedence <- c(
    "Calendar years BP",
    "Calibrated radiocarbon years BP",
    "Radiocarbon years BP", "Varve years BP"
  )
  ids <- getids(x)
  cuid <- x@collectionunitid
  # Check the chronologies to make sure everything is okay:
  if (length(chronologies(x)) > 0) {
    # This pulls the chronology IDs, then applies the Neotoma
    # age model precedence (see get_table('agetypes')).
    # It returns a value that is larger when your age reporting is
    # better.
    defaultchron <- map(
      chronologies(x)@chronologies,
      function(y) {
        data.frame(
          chronologyid = y@chronologyid,
          isdefault = y@isdefault,
          modelagetype = y@modelagetype,
          chronologyname = y@chronologyname,
          dateprepared = y@dateprepared
        )
      }
    ) %>%
      bind_rows() %>%
      mutate(
        modelrank = match(.data$modelagetype, rev(precedence)),
        order = .data$isdefault * match(
          .data$modelagetype,
          rev(precedence)
        )
      )
    # Validation of default chrons, we want to check whether there
    # exists either multiple default chronologies for the same
    # time-frame or, alternately, no default chronology.
    all_na <- all(is.na(defaultchron$order))
    max_order <- suppressWarnings(max(defaultchron$order, na.rm = TRUE))
    if (sum(defaultchron$order == max_order, na.rm = TRUE) > 1) {
      if (any(is.na(defaultchron$dateprepared))) {
        high_chron <- defaultchron$order == max_order
        newmax_order <- which.max(defaultchron$chronologyid[high_chron])
        defaultchron$order[high_chron][newmax_order] <- max_order + 1
      } else {
        newmax_order <- which.max(defaultchron$dateprepared[
          defaultchron$order == max_order
        ])
        defaultchron$order[defaultchron$order == max_order][
          newmax_order
        ] <- max_order + 1
      }
    }
    # Did the user request a chronology by name that actually exists here?
    chron_exists <- !is.null(chronname) &&
      chronname %in% defaultchron$chronologyname
    if (all_na == TRUE) {
      # No usable default: take the first chronology so that a single
      # chronology id is compared against the sample ages below.
      defaultchron <- defaultchron[1, ]
      warning(
        sprintf(
          "Collection unit %s (dataset %s) has no default chronologies.",
          cuid, ids$datasetid[1]
        ),
        call. = FALSE
      )
    } else if (chron_exists) {
      # The user explicitly requested a chronology and it is present.
      matches <- defaultchron$chronologyname == chronname
      if (sum(matches, na.rm = TRUE) > 1) {
        defaultchron <- defaultchron[matches, ][1, ]
        warning(
          sprintf(
            paste0(
              "Collection unit %s (dataset %s) has multiple chronologies ",
              "matching chronname '%s'. Chronology %s has been used."
            ),
            cuid, ids$datasetid[1], chronname, defaultchron$chronologyid
          ),
          call. = FALSE
        )
      } else {
        defaultchron <- defaultchron[matches, ]
      }
    } else if (sum(defaultchron$order == max_order, na.rm = TRUE) > 1) {
      defaultchron <- defaultchron[which.max(defaultchron$order), ]
      warning(
        sprintf(
          paste0(
            "Collection unit %s (dataset %s) has multiple default ",
            "chronologies. Chronology %s has been used."
          ),
          cuid, ids$datasetid[1], defaultchron$chronologyid
        ),
        call. = FALSE
      )
    } else {
      # Includes a `chronname` this unit does not have: the default is used
      # and reported through `chronname_matched` (one warning is raised by
      # the calling `samples()` method, not one per collection unit).
      defaultchron <- defaultchron[which.max(defaultchron$order), ]
    }
  } else {
    defaultchron <- data.frame(chronologyid = NULL)
  }
  sampset <- map(
    datasets(x)@datasets,
    function(y) {
      dsid <- y$datasetid
      allsamp <-
        map(
          y@samples@samples,
          function(z) {
            whichage <- which(
              z@ages$chronologyid == defaultchron$chronologyid
            )
            if (length(whichage) == 0) {
              # When a specific chronology was requested but is
              # absent for this sample, report NA so the sample
              # is dropped downstream; otherwise fall back to
              # the first age row. It must be an integer NA: a logical
              # NA is recycled to every row of the ages table.
              if (chron_exists) {
                whichage <- NA_integer_
              } else {
                whichage <- 1
              }
            }
            if (dim(z@datum)[1] > 0) {
              df <-
                data.frame(z@ages[whichage, ],
                  z@datum,
                  analysisunitid = z@analysisunitid,
                  sampleanalyst = toString(unique(unlist(
                    z@sampleanalyst,
                    use.names = FALSE
                  ))),
                  sampleid = z@sampleid,
                  depth = z@depth,
                  thickness = z@thickness,
                  samplename = z@samplename,
                  row.names = NULL
                )
            } else {
              df <- data.frame()
            }
            df
          }
        ) %>%
        bind_rows() %>%
        mutate(datasetid = dsid)
      allsamp
    }
  ) %>%
    bind_rows() %>%
    left_join(as.data.frame(datasets(x)), by = "datasetid") %>%
    rename(datasetnotes = "notes")
  if (chron_exists) {
    # The named chronology is present: drop samples that do not carry it
    # (their ages were set to NA above).
    sampset <- sampset %>% dplyr::filter(!is.na(.data$chronologyid))
  }
  if (!is.null(chronname)) {
    # TRUE where the age comes from the requested chronology, FALSE where this
    # collection unit does not have it and the default was used instead.
    sampset$chronname_matched <- rep(chron_exists, nrow(sampset))
  }
  sampset
}
