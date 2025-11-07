#' @title Get object IDs
#' @author Simon Goring \email{goring@wisc.edu}
#' @description This function parses a site object, from \code{site} to
#' \code{dataset} level and returns a \code{data.frame} that contains the
#' site, collectionunit and dataset IDs for each element within the site.
#' @importFrom purrr map
#' @importFrom dplyr arrange bind_rows mutate
#' @param x A Neotoma2 \code{sites} or \code{collunits} object.
#' @param order sort items by `siteid`, `collunitid`, `datasetid`
#' @returns `data.frame` containing `siteid`, `datasetid`, and `collunitid`
#' @examples \donttest{
#' marion <- get_sites(sitename = "Marion Lake")
#' collunitids <- getids(collunits(marion))
#' }
#' @md
#' @export
getids <- function(x, order = TRUE) {
  if (!missing(x)) {
    UseMethod("getids", x)
  }
}

#' @rdname getids
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
      }) %>%
        bind_rows()
    } else {
      data.frame(collunitid = NA, datasetid = NA)
    }
    return(data.frame(siteid = siteid, collunits))
  })
  siteids <- do.call("rbind.data.frame", args = siteids)
  rownames(siteids) <- seq_len(nrow(siteids))
  if (order) {
    siteids <- siteids %>%
      arrange(siteid, collunitid, datasetid)
  }
  # Guaranteeing that future joins all work out
  siteids <- siteids %>%
    mutate(siteid = siteid,
           collunitid = collunitid,
           datasetid = datasetid)
  siteids <- siteids[!is.na(siteids$datasetid), ]
  return(siteids)
}

#' @rdname getids
#' @export
getids.site <- function(x, order = TRUE) {
  siteid <- x@siteid
  if (length(x@collunits) > 0) {
    collunits <- map(x@collunits@collunits, function(z) {
      collunitid <- z@collectionunitid
      if (length(z@datasets) > 0) {
        datasetids <- map(z@datasets@datasets, function(a) {
          a@datasetid
        })
      } else {
        datasetids <- NA
      }
      return(data.frame(collunitid = (collunitid),
                        datasetid = (unlist(datasetids))))
    }) %>%
      bind_rows()
  } else {
    data.frame(collunitid = NA, datasetid = NA)
  }
  return(data.frame(siteid = (siteid), collunits))
}

#' @rdname getids
#' @export
getids.collunits <- function(x, order = TRUE) {
  siteid <- NA
  if (length(x) > 0) {
    collunits <- map(x@collunits, function(z) {
      collunitid <- z@collectionunitid
      if (length(z@datasets) > 0) {
        datasetids <- map(z@datasets@datasets, function(a) {
          a@datasetid
        })
      } else {
        datasetids <- NA
      }
      return(data.frame(collunitid = (collunitid),
                        datasetid = (unlist(datasetids))))
    }) %>%
      bind_rows()
  } else {
    data.frame(collunitid = NA, datasetid = NA)
  }
  return(data.frame(siteid = (siteid), collunits))
}

#' @rdname getids
#' @export
getids.collunit <- function(x, order = TRUE) {
  siteid <- NA
  collunitid <- x@collectionunitid
  if (length(x@datasets) > 0) {
    datasetids <- map(x@datasets@datasets, function(a) {
      a@datasetid
    })
  } else {
    datasetids <- NA
  }
  df <- data.frame(siteid = siteid,
                    collunitid = collunitid,
                    datasetid = datasetids)
  df <- df[!is.na(df$datasetid), ]
  return(df)
}