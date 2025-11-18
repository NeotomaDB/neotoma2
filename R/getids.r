#' @title Get object IDs
#' @name getids
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
#' tryCatch({
#' marion <- get_sites(sitename = "Marion Lake")
#' collunitids <- getids(collunits(marion))
#' }, error = function(e) {
#'  message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @md
#' @export
getids <- function(x, order = TRUE) {
  UseMethod("getids")
}

#' @rdname getids
#' @method getids sites
#' @export getids.sites
#' @exportS3Method getids sites
getids.sites <- function(x, order = TRUE) {
  siteids <- map(x@sites, function(y) {
    siteid <- y@siteid
    if (length(y@collunits) > 0) {
      cuids <- map(y@collunits@collunits, function(z) {
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
      cuids <- data.frame(collunitid = NA, datasetid = NA)
    }
    return(data.frame(siteid = siteid, cuids))
  })
  siteids <- do.call("rbind.data.frame", args = siteids)
  rownames(siteids) <- seq_len(nrow(siteids))
  if (order) {
    siteids <- siteids %>%
      arrange(.data$siteid, .data$collunitid, .data$datasetid)
  }
  # Guaranteeing that future joins all work out
  siteids <- siteids %>%
    mutate(siteid = .data$siteid,
           collunitid = .data$collunitid,
           datasetid = .data$datasetid)
  siteids <- siteids[!is.na(siteids$datasetid), ]
  siteids <- unique(siteids)
  return(siteids)
}

#' @rdname getids
#' @method getids site
#' @exportS3Method neotoma2::getids site
getids.site <- function(x, order = TRUE) {
  siteid <- x@siteid
  if (length(x@collunits) > 0) {
    cuids <- map(x@collunits@collunits, function(z) {
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
    cuids <- data.frame(collunitid = NA, datasetid = NA)
  }
  return(data.frame(siteid = (siteid), cuids))
}

#' @rdname getids
#' @method getids collunits
#' @exportS3Method getids collunits
getids.collunits <- function(x, order = TRUE) {
  siteid <- NA
  if (length(x) > 0) {
    cuids <- map(x@collunits, function(z) {
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
    cuids <- data.frame(collunitid = NA, datasetid = NA)
  }
  return(data.frame(siteid = (siteid), cuids))
}

#' @rdname getids
#' @method getids collunit
#' @exportS3Method getids collunit
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