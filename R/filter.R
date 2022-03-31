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

  ids <- getids(x) %>%
    inner_join(as.data.frame(datasets(x)), by = "datasetid") %>%
    inner_join(as.data.frame(x), by = "siteid") %>%
    rename(datasetnotes = .data$notes.x,
           sitenotes = .data$notes.y,
           ageolder = .data$age_range_old,
           ageyounger = .data$age_range_young)

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
