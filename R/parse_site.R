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
parse_site <- function(result, verbose = FALSE, parse_download = FALSE) {
  data <- result$data
  data <- group_response(data)
  
  new_sites <- purrr::map(data, function(x) {
    if (verbose) {
      cat(".")
    }
    # Map collection units
    if (is.null(x$site$collectionunits)) {
      call <- x$collectionunits
    } else {
      call <- x$site$collectionunits
    }
    cus <- purrr::map(call, function(y){
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
                     age_range_old = use_na(z$agerange$ageold, "int"),
                     age_range_young = use_na(z$agerange$ageyoung, "int"),
                     age_units = use_na(z$agerange$units, "int"),
                     notes = use_na(z$datasetnotes, "char"),
                     pi_list = z$pi_list,
                     samples = samp,
                     specimens = NULL)
        do.call(build_dataset, ds_l)
      })
      
      ds <- new("datasets", datasets = ds)
      # Chronologies
      chronologies <- purrr::map(y$chronologies, function(z) {
        if (!is.null(z$chronology$chronologyid)){
          ch_l <- list(chronologyid = use_na(z$chronology$chronologyid, "int"),
                       notes = use_na(z$chronology$chronolgy$notes, "char"),
                       contact = use_na(z$chronology$chronolgy$contact, "char"),
                       agemodel = use_na(z$chronology$chronolgy$agemodel, "char"),
                       ageboundolder = use_na(z$chronology$chronolgy$ageboundolder, "int"),
                       ageboundyounger = use_na(z$chronology$chronolgy$ageboundyounger, "int"),
                       isdefault = use_na(z$chronology$chronology$isdefault, "bool"),
                       dateprepared = use_na(as.Date(z$chronology$chronology$dateprepared), "date"),
                       modelagetype = use_na(z$chronology$chronology$modelagetype, "char"),
                       chronologyname = use_na(z$chronology$chronology$chronologyname, "char"),
                       chroncontrols = z$chronology$chroncontrols)
          do.call(build_chron, ch_l)
        } else {
          NULL
        }
      })
      if (is.null(chronologies) || all(sapply(chronologies, is.null))) {
        chron <- new("chronologies", chronologies = list())
      } else {
        chron <- new("chronologies", chronologies = chronologies)
      }
      cu_l <- list(
        collectionunitid = y$collectionunitid,
        colldate = as.Date(testNull(y$colldate, NA)),
        handle = use_na(y$handle, "char"),
        datasets = ds,
        chronologies = chron,
        location = use_na(y$location, "char"),
        waterdepth = use_na(y$waterdepth, "int"),
        gpslocation = testNull(y$gpslocation, NA),
        collunittype = use_na(testNull(testNull(y$unittype, y$collunittype),
                                                y$collectionunittype), "char"),
        collectiondevice = use_na(y$collectiondevice, "char"),
        collectionunitname = use_na(y$collectionunit, "char"),
        depositionalenvironment = use_na(y$depositionalenvironment, "char"),
        defaultchronology = use_na(y$defaultchronology, "int"))
      do.call(build_collunits, cu_l)
    })
    cu <- new("collunits", collunits = cus)
    st <- if (!is.null(x$site)) x$site else x
    st_l <- list(sitename = st$sitename,
                 siteid = st$siteid,
                 geography = st$geography,
                 altitude = st$altitude,
                 description = st$sitedescription,
                 notes = st$sitenotes,
                 collunits = cu)
    do.call(build_site, st_l)
  })
  new_sites <- new("sites", sites = new_sites)
  return(new_sites)
}
normalize_agerange <- function(agerange) {
  if (is.null(agerange) || length(agerange) == 0) {
    list(ageold = NA, ageyoung = NA, units = NA)
  } else {
    list(ageold = agerange[[1]]$ageold, ageyoung = agerange[[1]]$ageyoung, units = agerange[[1]]$units)
  }
}