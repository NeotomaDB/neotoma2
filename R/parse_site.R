#' @title parse_site
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom purrr map
#' @importFrom methods new
#' @description An internal helper function used to parse site information into
#' `neotoma2R` objects.
#' @param result The API response.
#' @param verbose If TRUE print progress to console in bar form.
#' @returns `list` with cleaned and parsed data from HTTP request
#' @noRd
parse_site <- function(result, verbose = FALSE) {
  data <- result$data
  data <- group_response(data)
  new_sites <- map(data, function(x) {
    if (verbose) {
      cat(".")
    }
    # Collection units
    if (is.null(x$site$collectionunits)) {
      call <- x$collectionunits
    } else {
      call <- x$site$collectionunits
    }
    cus <- map(call, function(y){
      # Datasets
      ds <- map(y$datasets, function(z) {
        samp <- map(z$samples, build_sample)
        samp <- new("samples", samples = samp)
        z$agerange <- normalize_agerange(z$agerange)
        ds_l <- list(datasetid = z$datasetid,
                     database = use_na(z$database, "char"),
                     doi = z$doi,
                     recdatecreated = use_na(as.Date(z$recdatecreated), "date"),
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
      chronologies <- map(y$chronologies, function(z) {
        if (!is.null(z$chronology$chronologyid)) {
          ch <- z$chronology
          ch_l <-
            list(chronologyid = use_na(ch$chronologyid, "int"),
                 notes = use_na(ch$chronolgy$notes, "char"),
                 contact = use_na(ch$chronolgy$contact, "char"),
                 agemodel = use_na(ch$chronolgy$agemodel, "char"),
                 ageboundolder = use_na(ch$chronolgy$ageboundolder, "int"),
                 ageboundyounger = use_na(ch$chronolgy$ageboundyounger, "int"),
                 isdefault = use_na(ch$chronology$isdefault, "bool"),
                 dateprepared = use_na(as.Date(ch$chronology$dateprepared),
                                       "date"),
                 modelagetype = use_na(ch$chronology$modelagetype, "char"),
                 chronologyname = use_na(ch$chronology$chronologyname, "char"),
                 chroncontrols = ch$chroncontrols)
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
      # Speleothems
      speleothems <- tryCatch(
        parse_speleothem(y$speleothems),
        error = function(e) {
          NULL # Speleothems does not exist in the regular API calls
        }
      )
      # Build collection unit
      cu_l <-
        list(collectionunitid = y$collectionunitid,
             colldate = as.Date(use_na(y$colldate, "date")),
             handle = use_na(y$handle, "char"),
             datasets = ds,
             chronologies = chron,
             location = use_na(y$location, "char"),
             waterdepth = use_na(y$waterdepth, "int"),
             gpslocation = use_na(y$gpslocation, "sf"),
             collunittype = use_na(y$collectionunittype, "char"),
             collectiondevice = use_na(y$collectiondevice, "char"),
             collectionunitname = use_na(y$collectionunit, "char"),
             depositionalenvironment = use_na(y$depositionalenvironment,
                                              "char"),
             defaultchronology = use_na(y$defaultchronology, "int"),
             speleothems = speleothems)
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
  new_sites <- clean(new_sites)
  return(new_sites)
}

#' @title normalize_agerange
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal helper function used to parse age range information
#' @param agerange age range information from API response
#' @returns list with normalized age range information
#' @noRd
normalize_agerange <- function(agerange) {
  if (is.null(agerange) || length(agerange) == 0) {
    list(ageold = NA, ageyoung = NA, units = NA)
  } else {
    list(ageold = agerange[[1]]$ageold,
         ageyoung = agerange[[1]]$ageyoung,
         units = agerange[[1]]$units)
  }
}