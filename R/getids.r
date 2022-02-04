#' @title Get object IDs
#' @param x A neotoma2 \code{sites} object.
#' @description This function parses a site object, from \code{site} to
#' \code{dataset} level and returns a \code{data.frame} that contains the
#' site, collectionunit and dataset IDs for each element within the site.
#' @importFrom purrr map
#' @importFrom dplyr arrange
#' @export
getids <- function(x) {
  if (!missing(x)) {
    UseMethod("getids", x)
  }
}

#' @title Get object IDs from sites
#' @param x A neotoma2 \code{sites} object.
#' @export
getids.sites <- function(x) {
    siteids <- map(x@sites, function(y) {
        siteid <- y@siteid
        if (length(y@collunits) > 0) {
            collunits <- map(y@collunits@collunits, function(z) {
                collunitid <- z@collectionunitid
                if (length(z@datasets) > 0) {
                    datasetids <- map(z@datasets@datasets, function(a) {
                        a@datasetid
                    })
                } else {
                    datasetids <- NA
                }

                return(data.frame(collunitid = collunitid,
                                datasetid = unlist(datasetids)))
            })
        } else {
            data.frame(collunitid = NA, datasetid = NA)
        }
        return(data.frame(siteid = siteid, collunits))
    }) %>%
    bind_rows() %>%
    arrange(siteid, collunitid, datasetid)
    return(siteids)
}