#' @title group_data
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map map_lgl flatten imap
#' @description An internal helper function used to parse the
#' API response into a hierarchical list of
#' *sites* > *collectionunits* > *datasets*
#' @param data API response
#' @returns hierarchical `list` with parsed API request
#' @noRd
group_response <- function(data) {
  # Detect endpoint
  is_dataset  <- all(map_lgl(data, ~ !is.null(.x$site$datasets)))
  is_download <- all(map_lgl(data, function(x) {
    tryCatch(!is.null(x$site$collectionunit$dataset), error = function(e) FALSE)
  }))
  is_site <- all(map_lgl(data, ~ !is.null(.x$collectionunits)))
  if (is_download) {
    return(group_download(data))
  }
  if (is_dataset) {
    return(group_dataset(data))
  }
  if (is_site) {
    return(data)
  } else {
    stop("Unrecognized data structure.")
  }
}

#' @title group_download
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal function used to parse the 
#' API download object response
#' @param data API response
#' @returns hierarchical `list` with parsed API request
#' @noRd
group_download <- function(data) {
  site_groups <- split(data, sapply(data, function(x) x$site$siteid))
  map(site_groups, function(site_group) {
    site_info <- site_group[[1]]$site
    # collection units
    cu_groups <- split(site_group, sapply(site_group, function(x) {
      x$site$collectionunit$collectionunitid
    }))
    collectionunits <- imap(cu_groups, function(cu_group, cu_id) {
      cu_info <- cu_group[[1]]$site$collectionunit
      # datasets
      cu_info$dataset <- NULL
      cu_info$datasets <- map(cu_group, ~ .x$site$collectionunit$dataset)
      cu_info
    })
    site_info$dataset <- NULL
    site_info$collectionunit <- NULL
    site_info$collectionunits <- collectionunits
    list(site = site_info)
  })
}

#' @title group_download
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal function used to parse the 
#' API dataset object response
#' @param data API response
#' @returns hierarchical `list` with parsed API request
#' @noRd
group_dataset <- function(data) {
  site_groups <- split(data, sapply(data, function(x) x$site$siteid))
  sites <- map(site_groups, function(site_group) {
    site_info <- site_group[[1]]$site
    # collection units
    cu_groups <- split(site_group, sapply(site_group,
                       function(x) x$site$collectionunitid))
    collunits <- map(cu_groups, function(cu_group) {
      cu_info <- cu_group[[1]]$site
      # datasets in cu
      all_datasets <- map(cu_group, function(x) x$site$datasets) %>%
        flatten()
      cu_info$datasets <- all_datasets
      cu_info
    })
    # Remove unnecessary fields
    site_info$datasets <- NULL
    site_info$handle <- NULL
    site_info$collectionunitid <- NULL
    site_info$collectionunit <- NULL
    site_info$collectionunits <- collunits
    list(site = site_info)
  })
  return(sites)
}