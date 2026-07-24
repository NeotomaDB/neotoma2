#' @title clean Neotoma objects to remove duplicates and empty objects.
#' @name clean
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom progress progress_bar
#' @importFrom methods new
#' @param x sites, datasets, collunits that may have duplicates.
#' @param verbose parameter to prints out progress bar
#' @param ... Additional parameters associated with the call.
#' @returns clean `neotoma` objects without duplicates after concatenation
#' @description Function that removes duplicate objects such as sites,
#' datasets, or collection units. When we pull in a large number of objects,
#' or overlapping searches, we can run into a problem where we have multiple
#' instances of the same site, but with different datasets. This function
#' attempts to gather all objects together:
#'   * Before: \{site: 1, dataset: 1\}, \{site: 1, dataset: 2\}
#'   * After: \{site: 1, dataset: \[1, 2\]\}
#' So the site is gathered, and the datasets are now part of an
#' array of datasets.
#' @examples \dontrun{
#' tryCatch({
#'   alex <- get_sites(sitename = "Alex%")
#'   alex2 <- get_sites(24)
#'   c <- c(alex, alex2) #uncleaned
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @md
#' @export
clean <- function(x, verbose = TRUE, ...) {
  UseMethod("clean")
}

#' @rdname clean
#' @method clean sites
#' @exportS3Method clean sites
clean.sites <- function(x, verbose = TRUE, ...) {
  # Work directly on the site objects. Using `as.data.frame(x)$siteid` /
  # `filter()` here is unsafe: `as.data.frame()` yields one row per collection
  # unit (so a single multi-collunit site looks duplicated) and `filter()`
  # builds its index from `getids()`, which drops sites that carry no datasets
  # -- those sites then become invisible and slip through de-duplication,
  # eventually producing an invalid (empty) `sites` object.
  siteids <- vapply(x@sites, function(s) as.numeric(s@siteid), numeric(1))
  matched <- unique(siteids[duplicated(siteids)])
  if (length(matched) == 0) {
    return(x)
  }
  # Sites with a unique siteid are kept unchanged, in their original order.
  clean_list <- x@sites[!(siteids %in% matched)]
  pb <- progress_bar$new(total = length(matched))
  for (i in matched) {
    if (verbose == TRUE) {
      pb$tick()
    }
    dup_sites <- x@sites[siteids == i]
    merged <- dup_sites[[1]]
    # Gather every collection unit from the duplicate site instances and
    # de-duplicate them (this also merges their datasets). Some instances may
    # carry no collection units, which is handled gracefully.
    all_cus <- unlist(lapply(dup_sites, function(s) s@collunits@collunits),
                      recursive = FALSE)
    merged@collunits <- clean(new("collunits", collunits = all_cus),
                              verbose = FALSE)
    clean_list <- c(clean_list, merged)
  }
  return(new("sites", sites = clean_list))
}

#' @rdname clean
#' @method clean collunits
#' @exportS3Method clean collunits
clean.collunits <- function(x, verbose = TRUE, ...) {
  cuids <- x$collectionunitid
  matched <- unique(cuids[duplicated(cuids)])
  non_dupes <- cuids[!duplicated(cuids) & !duplicated(cuids, fromLast = TRUE)]
  if (length(matched) == 0) {
    return(x)
  } else {
    clean_cus <- x[which(!cuids %in% matched)]
    for (i in matched) {
      messy_cus <- new("collunits", collunits = x@collunits[cuids == i])
      cu_ds <- clean(datasets(messy_cus))
      newcu <- messy_cus[1]
      newcu[[1]]@datasets <- cu_ds
      clean_cus <- c(clean_cus, newcu)
    }
  }
  return(clean_cus)
}

#' @rdname clean
#' @method clean datasets
#' @exportS3Method clean datasets
clean.datasets <- function(x, verbose = TRUE, ...) {
  dsids <- as.data.frame(x)$datasetid
  return(x[which(!duplicated(dsids))])
}