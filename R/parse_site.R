#' @md
#' @title parseURL
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @import stringr
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @param x The HTTP/S path for the particular API call.
#' @param use Uses the Neotoma server by default ("neotoma"),
#' but supports either the development API server ("dev"),
#' or a local server ("local").
#' @param all_data If TRUE return all possible API calls
#' @param ... Any query parameters passed from the calling function.
#' @returns `list` with cleaned and parsed data from HTTP request
#' @keywords internal
#' @noRd
parse_site <- function(result, parse_download = FALSE) {
  data <- result$data
  data <- group_data(data)
  
  new_sites <- purrr::map(data, function(x) {
    # Map collection units
    cu <- purrr::map(x$site$collectionunits, function(y){
      # Map datasets
      ds <- purrr::map(y$datasets, function(z) {
        samp <- purrr::map(z$samples, build_sample)
        samp <- new("samples", samples = samp)
        z$agerange <- normalize_agerange(z$agerange)
        ds_l <- list(datasetid = z$datasetid,
             database = use_na(z$database, "char"),
             doi = z$doi,
             datasettype = use_na(z$datasettype, "char"),
             datasetname = use_na(z$datasetname, "char"),
             age_range_old = use_na(z$agerange[[1]]$ageold, "int"),
             age_range_young = use_na(z$agerange[[1]]$ageyoung, "int"),
             age_units = use_na(z$agerange[[1]]$units, "int"),
             notes = use_na(z$datasetnotes, "char"),
             pi_list = z$pi_list,
             samples = samp,
             specimens = NULL)
        do.call(build_dataset, ds_l)
      })
      # TODO Build Chronologies
      ds <- new("datasets", datasets = ds)
      cu_l <- list(
           collectionunitid = y$collectionunitid,
           colldate = as.Date(testNull(y$colldate, NA)),
           handle = use_na(y$handle, "char"),
           datasets = ds,
           chronologies = NULL,
           location = use_na(y$location, "char"),
           waterdepth = use_na(y$waterdepth, "int"),
           gpslocation = testNull(y$gpslocation, NA),
           collunittype = use_na(y$collunittype, "char"),
           collectiondevice = use_na(y$collectiondevice, "char"),
           collectionunitname = use_na(y$collectionunitname, "char"),
           depositionalenvironment = use_na(y$depositionalenvironment, "char"),
           defaultchronology = use_na(y$defaultchronology, "int"))
      do.call(build_collunits, cu_l)
    })
    cu <- new("collunits", collunits = cu)
    st_l <- list(sitename = x$site$sitename,
                     siteid = x$site$siteid,
                     geography = x$site$geography,
                     altitude = x$site$altitude,
                     description = x$site$sitedescription,
                     notes = x$site$sitenotes,
                     collunits = cu)
    do.call(build_site, st_l)
  })
  new_sites <- new("sites", sites = new_sites)
  return(new_sites)
}

normalize_agerange <- function(agerange) {
  if (is.null(agerange) || length(agerange) == 0) {
    list(list(ageold = NA, ageyoung = NA, units = NA))
  } else {
    agerange
  }
}