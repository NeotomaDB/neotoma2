#' @title Get object IDs
#' @param x A Neotoma2 \code{sites} object.
#' @description This function parses a site object, from \code{site} to
#' \code{dataset} level and returns a \code{data.frame} that contains the
#' site, collectionunit and dataset IDs for each element within the site.
#' @importFrom purrr map
#' @importFrom dplyr arrange
#' @param order sort items by siteid, collunitid, datasetid
#' @returns `data.frame` containing `siteid`, `datasetid`, and `collunitid`
#' @export
getids <- function(x, order = TRUE) {
  if (!missing(x)) {
    UseMethod("getids", x)
  }
}

#' @title Get object IDs from sites
#' @param x A Neotoma2 \code{sites} object.
#' @param order sort items by siteid, collunitid, datasetid
#' @importFrom purrr map
#' @importFrom dplyr arrange bind_rows
#' @returns `data.frame` containing `siteid`, `datasetid`, and `collunitid`
#' @export
getids.sites <- function (x, order = TRUE) {
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
            }) %>% dplyr::bind_rows()
        } else {
            data.frame(collunitid = NA, datasetid = NA)
        }
        return(data.frame(siteid = siteid, collunits))
    }) #%>%

    siteids <- do.call("rbind.data.frame", args = siteids)
    rownames(siteids) <- seq(length = nrow(siteids))

    if (order) {
      siteids <- siteids %>%
        arrange(siteid, collunitid, datasetid)
    }

    # Guaranteeing that future joins all work out
    siteids <- siteids %>% 
      mutate(siteid = as.character(siteid),
             collunitid = as.character(collunitid),
             datasetid = as.character(datasetid))

    return(siteids)
}

#' @title Get object IDs from a site object.
#' @param x A Neotoma2 \code{site} object.
#' @param order sort items by siteid, collunitid, datasetid
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @returns `data.frame` containing `siteid`, `datasetid`, and `collunitid`
#' @export
getids.site <- function (x, order = TRUE) {
  siteid <- x@siteid
  if (length(x@collunits) > 0) {
    collunits <- purrr::map(x@collunits@collunits, function(z) {
      collunitid <- z@collectionunitid
      if (length(z@datasets) > 0) {
        datasetids <- purrr::map(z@datasets@datasets, function(a) {
          a@datasetid
        })
      } else {
        datasetids <- NA
      }

      return(data.frame(collunitid = as.character(collunitid),
                        datasetid = as.character(unlist(datasetids))))
    }) %>% dplyr::bind_rows()
  } else {
    data.frame(collunitid = NA, datasetid = NA)
  }
  return(data.frame(siteid = as.character(siteid), collunits))
}

#' @title Get object IDs from collectionunits.
#' @description From a set of `collectionunit` objects, return the
#' collectionunit and dataset ids.
#' @param x A Neotoma2 \code{collunits} object.
#' @param order sort items by siteid, collunitid, datasetid
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @examples \donttest{
#' marion <- get_sites(sitename = "Marion Lake")
#' collunitids <- getids(collunits(marion))
#' }
#' @returns `data.frame` containing `siteid`, `datasetid`, and `collunitid`
#' @export
getids.collunits <- function(x, order = TRUE) {

  siteid <- NA
  if (length(x) > 0) {
    collunits <- purrr::map(x@collunits, function(z) {
      collunitid <- z@collectionunitid
      if (length(z@datasets) > 0) {
        datasetids <- purrr::map(z@datasets@datasets, function(a) {
          a@datasetid
        })
      } else {
        datasetids <- NA
      }

      return(data.frame(collunitid = as.character(collunitid),
                        datasetid = as.character(unlist(datasetids))))
    }) %>% dplyr::bind_rows()
  } else {
    data.frame(collunitid = NA, datasetid = NA)
  }
  return(data.frame(siteid = as.character(siteid), collunits))
}

#' @title Get object IDs from a single collectionunit.
#' @description From a `collectionunit` object, return the
#' collectionunit and dataset ids.
#' @param x A Neotoma2 \code{collunit} object.
#' @param order sort items by siteid, collunitid, datasetid
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @returns `data.frame` containing `siteid`, `datasetid`, and `collunitid`
#' @examples \donttest{
#' marion <- get_sites(sitename = "Marion Lake") 
#' collunitids <- getids(collunits(marion)[[1]])
#' }
#' @export
getids.collunit <- function(x, order = TRUE) {

  siteid <- NA
  collunitid <- x@collectionunitid

  if (length(x@datasets) > 0) {
    datasetids <- purrr::map(x@datasets@datasets, function(a) {
      a@datasetid
    })
  } else {
    datasetids <- NA
  }

  return(data.frame(siteid = as.character(siteid),
                    collunitid = as.character(collunitid),
                    datasetid = as.character(datasetids)))
}
