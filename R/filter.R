#' @title Apply a `filter` for Neotoma sites objects.
#' @name filter
#' @author Simon Goring \email{goring@wisc.edu}
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description The \code{filter} function takes a \code{sites} object
#' and allows a user to filter on a number of properties. Since a sites object
#' is a nested object (it contains collection units, datasets, samples, etc.)
#' the degree to which filtering occurs depends on the amount of data contained
#' within the sites object. Filtering parameters include:
#'  * `siteid` A numeric site identifier from the Neotoma Database.
#'  * `sitename` The character string sitename.
#'  * `lat` A numeric latitude value.
#'  * `long` A numeric longitude value.
#'  * `altitude` The elevation of the site. Note that some sites do not
#'   include elevation information. For these an NA value appears, and they
#'   would be removed when using an elevation filter.
#'  * `datasetid` A numeric datasetid from Neotoma.
#'  * `database` A character string naming the constituent database
#'   from which the dataset is drawn.
#'  * `datasettype` A character string representing one of the many
#'   dataset types within Neotoma.
#'  * `age_range_old` A dataset-level parameter indicating the oldest
#'   date covered by the dataset chronology.
#'  * `age_range_young` A dataset-level parameter indicating the youngest
#'   date covered by the dataset chronology.
#'  * `notes` Free-form dataset notes provided by the dataset PI(s),
#'   analysts or data stewards.
#'  * `collectionunitid` A numeric collection unit identifier from
#'   Neotoma.
#'  * `handle` A character string identifying the collection unit. These
#'   are often shorter form names (originally a default 8 character length).
#'  * `collectionunitname` A character string identifying the collection
#'   unit name.
#'  * `colldate` The date on which the collection unit was sampled. Many
#'   of these are empty.
#'  * `location` A free-form character string indicating the location of
#'   the collection unit within the site.
#'  * `waterdepth` A numeric depth at which the core was obtained.
#'  * `collunittype` A character string for the collection unit type.
#'  * `collectiondevice` A fixed vocabulary term for the collection
#'   device.
#'  * `depositionalenvironment` A fixed vocabulary name for the
#'   depositional environment.
#' @importFrom dplyr filter inner_join
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @param .data A site, dataset, download, or data frame
#' @param ... Additional arguments passed to `filter()`
#' @param .by (only used for filtering `data.frame` objects)
#' @param .preserve (only used for filtering `data.frame` objects)
#' @returns filtered `sites` object
#' @examples \donttest{
#' # Download 10 sites, but only keep the sites that are close to sea level.
#' tryCatch({
#'  some_sites <- get_sites(sitename = "Lake%", limit = 3)
#'   site_subset <- some_sites %>% filter(altitude < 100)
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#' # Download 10 sites, get all associated datasets, but keep only
#' # sites/datasets that are of datasettype "pollen":
#' tryCatch({
#'   sites <- get_sites(limit = 10) %>%
#'     get_datasets()
#'   pollen_subset <- sites %>% filter(datasettype == "pollen")
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.") 
#' })
#' }
#' @md
#' @export
filter <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  UseMethod("filter")
}

#' @rdname filter
#' @exportS3Method filter NULL
filter.NULL <- function(.data, ...) {
  warning("No sites to filter")
  return(NULL)
}

#' @rdname filter
#' @exportS3Method filter sites
filter.sites <- function(.data, ...) {
  x <- .data
  ellipsis <- as.list(substitute(list(...), environment()))[-1L][[1]] %>%
    as.character()
  sitecols <- c("sitename", "lat", "long", "altitude") %>%
    map(function(x) any(stringr::str_detect(ellipsis, x))) %>%
    unlist() %>%
    any()
  datasetcols <- c("datasetid", "database", "datasettype", "age_range_old",
                   "age_range_young", "notes") %>%
    map(function(x) any(stringr::str_detect(ellipsis, x))) %>%
    unlist() %>%
    any()
  collunitcols <- c("collectionunitid", "handle", "colldate",
                    "location", "waterdepth", "collunittype",
                    "collectiondevice", "defaultchronology",
                    "collectionunitname", "depositionalenvironment") %>%
    map(function(x) any(stringr::str_detect(ellipsis, x))) %>%
    unlist() %>%
    any()
  ids <- getids(x)
  if (sitecols == TRUE) {
    ids <- ids %>%
      inner_join(as.data.frame(x), by = "siteid") %>%
      rename(altitude = .data$elev,
             sitenotes = .data$notes)
  }
  if (collunitcols == TRUE) {
    ids <- ids %>%
      inner_join(as.data.frame(collunits(x)),
                 by = c("collunitid" = "collectionunitid"))
  }
  if (datasetcols == TRUE) {
    ids <- ids %>%
      inner_join(mutate(as.data.frame(datasets(x)),
                        datasetid = as.numeric(.data$datasetid)),
                 by = "datasetid")
  }
  cleanids <- ids %>%
    dplyr::filter(...)
  if (nrow(cleanids) == 0) {
    return(new("sites"))
  }
  siteids <- unique(as.data.frame(x)$siteid)
  pared_sites <- x[which(siteids %in% cleanids$siteid)]
  # Clear datasets:
  good_dsid <- unique(cleanids$datasetid)
  good_cuid <- unique(cleanids$collunitid)
  pared_ds <- purrr::map(pared_sites@sites, function(x) {
    ycu <- collunits(x)
    ycu <- ycu[which(as.data.frame(ycu)$collectionunitid %in% good_cuid)]
    xcu <- purrr::map(ycu@collunits, function(y) {
      yds <- datasets(y)
      yds <- yds[which(as.data.frame(yds)$datasetid %in% good_dsid)]
      y@datasets <- yds
      return(y)
    })
    x@collunits@collunits <- xcu
    return(x)
  })
  return(new("sites", sites = pared_ds))
}

#' This is a re-export of \code{dplyr::filter} for data frames.
#' @rdname filter
#' @exportS3Method filter data.frame
filter.data.frame <- getS3method("filter",
                                 "data.frame",
                                 envir = asNamespace("dplyr"))