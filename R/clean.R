utils::globalVariables(c("siteid"))

#' @title clean Neotoma objects to remove duplicates and empty objects.
#' @author Simon Goring \email{goring@wisc.edu}
#' @import gtools
#' @import lubridate
#' @importFrom progress progress_bar
#' @importFrom methods new
#' @param x sites, datasets, collunits that may have duplicates.
#' @param verbose parameter to prints out progress bar
#' @param ... Additional parameters associated with the call.
#' @description Function that removes duplicate objects such as sites,
#' datasets, or collection units.
#' @export
clean <- function(x = NA, verbose = TRUE, ...) {
  if (!missing(x)) {
    UseMethod("clean", x)
  } else {
    UseMethod("clean", NA)
  }
}

#' @title clean sites objects to remove duplicates.
#' @author Simon Goring \email{goring@wisc.edu}
#' @import gtools
#' @import lubridate
#' @importFrom progress progress_bar
#' @importFrom methods new
#' @param x sites, datasets, collunits that may have duplicates.
#' @param verbose parameter to prints out progress bar
#' @param ... Additional parameters associated with the call.
#' @description Function that removes duplicate objects such as sites,
#' datasets, or collection units.
#' @examples
#' clean_sites <- get_sites(sitename = "L%", limit = 20)
#' more_sites <- get_sites(sitename = "La%", limit = 20)
#' long_set <- c(clean_sites, more_sites)
#' length(long_set)
#' # By removing duplicates we get a smaller object.
#' length(clean(long_set))
#' @export
clean.sites <- function(x, verbose = TRUE) {

  siteids <- as.data.frame(x)$siteid

  if (any(duplicated(siteids))) {
    matched <- unique(siteids[duplicated(siteids)])

    clean_sites <- filter(x, !siteid %in% matched)
    messy_sites <- neotoma2::filter(x, siteid %in% matched)

    pb <- progress_bar$new(total = length(matched))

    for (i in matched) {
      if (verbose == TRUE) {
        pb$tick()
      }
      messy_site <- neotoma2::filter(messy_sites, siteid == i)
      messy_cus <- clean(collunits(messy_site))
      new_site <- messy_site[1]
      new_site@sites[[1]]@collunits <- messy_cus
      clean_sites <- c(clean_sites, new_site[[1]])
    }

    return(clean_sites)

  } else {
    return(x)
  }
}

#' @title clean sites objects to remove duplicates.
#' @author Simon Goring \email{goring@wisc.edu}
#' @import gtools
#' @import lubridate
#' @importFrom progress progress_bar
#' @importFrom methods new
#' @param x sites, datasets, collunits that may have duplicates.
#' @param verbose parameter to prints out progress bar
#' @param ... Additional parameters associated with the call.
#' @description Function that removes duplicate objects such as sites,
#' datasets, or collection units.
#' @examples
#' clean_cols <- get_sites(sitename = "L%", limit = 20) %>%
#'   collunits()
#' more_cols <- get_sites(sitename = "La%", limit = 20) %>%
#'   collunits()
#' long_set <- c(clean_cols, more_cols)
#' length(long_set)
#' # By removing duplicates we get a smaller object.
#' length(clean(long_set))
#' @export
clean.collunits <- function(x) {
  cuids <- as.data.frame(x)$collectionunitid
  matched <- unique(cuids[duplicated(cuids)])
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

#' @title clean sites objects to remove duplicates.
#' @author Simon Goring \email{goring@wisc.edu}
#' @import gtools
#' @import lubridate
#' @importFrom progress progress_bar
#' @importFrom methods new
#' @param x sites, datasets, collunits that may have duplicates.
#' @param verbose parameter to prints out progress bar
#' @param ... Additional parameters associated with the call.
#' @description Function that removes duplicate objects such as sites,
#' datasets, or collection units.
#' @examples
#' clean_ds <- get_sites(sitename = "L%", limit = 20) %>%
#'   get_downloads() %>% datasets()
#' more_ds <- get_sites(sitename = "La%", limit = 20) %>%
#'   get_downloads() %>% datasets()
#' long_set <- c(clean_ds, more_ds)
#' length(long_set)
#' # By removing duplicates we get a smaller object.
#' length(clean(long_set))
#' @export
clean.datasets <- function(x) {
  dsids <- as.data.frame(x)$datasetid
  return(x[which(!duplicated(dsids))])
}
