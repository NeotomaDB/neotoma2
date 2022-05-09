#' @title filter
#' @import sf
#' @import dplyr
#' @importFrom purrr map
#' @import stringr
#' @param x A site, dataset or download.
#' @param ... arguments to filter by: lat, long,
#' elev, datasettype
#' @export

filter <- function(x, ...) {
  UseMethod("filter", x)
}

#' @export
filter.sites <- function(x, ...) {  # nolint

  # It is time consuming to do all the joining.  So here we
  # do a thing to try to speed stuff up by only joining the stuff we need:
  ellipsis <- as.list(substitute(list(...), environment()))[-1L][[1]] %>%
    as.character()

  sitecols <- c('sitename', 'lat', 'long', 'altitude') %>%
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
                    "collectiondevice", "defaultchronology", "collectionunitname",
                    "depositionalenvironment") %>%
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

  if (datasetcols == TRUE) {
    ids <- ids %>%
      inner_join(as.data.frame(datasets(x)), by = "datasetid") %>%
      rename(datasetnotes = .data$notes)
  }

  if (collunitcols == TRUE) {
    ids <- ids %>%
      inner_join(as.data.frame(collunits(x)), by = c("collunitid" = "collectionunitid"))
  }

  cleanids <- ids %>%
    dplyr::filter(...)

  if (nrow(cleanids) == 0) {
    return(new("sites"))
  }

  siteids <- as.data.frame(x)$siteid

  pared_sites <- x[which(siteids %in% cleanids$siteid)]

  # Sites are cleared.  Now need to clear datasets:
  good_dsid <- unique(cleanids$datasetid)

  pared_ds <- purrr::map(pared_sites@sites, function(x) {
    xcu <- purrr::map(collunits(x)@collunits, function(y) {
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
