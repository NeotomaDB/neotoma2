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
parse_site <- function(result) {
  data <- result$data
  new_sites <- purrr::map(data, function(x) {
    if (!is.null(x$sites)) {
      call <- x$sites$site
    } else if (!is.null(x$site)) {
      call <- x$site
    } else {
      call <- x
    }
    # DSs
    if (!is.null(x$sites$datasets)) {
      ds_ <- x$sites$datasets
    } else if (!is.null(x$site$datasets)){
      ds_ <- x$site$datasets
    }else {
      ds_ <- x
    }
    if (!("collectionunits" %in% names(call))) {
      datasets_ <- purrr::map(ds_, function(x){
        if (is.null(x$agerange) || length(x$agerange) == 0){
          x$agerange <- list(list(ageold = NA, ageyoung = NA, units = NA))
        }
        ds <- list(datasetid = x$datasetid,
                   database = use_na(x$database, "char"),
                   doi = x$doi,
                   datasettype = use_na(x$datasettype, "char"),
                   datasetname = use_na(x$datasetname, "char"),
                   age_range_old = use_na(x$agerange[[1]]$ageold, "int"),
                   age_range_young = use_na(x$agerange[[1]]$ageyoung, "int"),
                   age_units = use_na(x$agerange[[1]]$units, "int"),
                   notes = use_na(x$datasetnotes, "char"),
                   pi_list = x$pi_list,
                   samples = NULL,
                   specimens = NULL)
        cuid <- pluck(call, "collectionunits", "collectionunitid", .default = 
                        pluck(call, "collectionunitid", .default = NA))
        list(collectionunitid = cuid , dataset = do.call(build_dataset, ds))
      })
    } else {
      datasets_ <- purrr::map(call$collectionunits, function(cu) {
        purrr::map(cu$datasets, function(x) {
          if (is.null(x$agerange) || length(x$agerange) == 0){
            x$agerange <- list(list(ageold = NA, ageyoung = NA, units = NA))
          }
          ds <- list(
            datasetid = x$datasetid,
            database = use_na(x$database, "char"),
            doi = x$doi,
            datasettype = use_na(x$datasettype, "char"),
            datasetname = use_na(x$datasetname, "char"),
            age_range_old = use_na(x$agerange[[1]]$ageold, "int"),
            age_range_young = use_na(x$agerange[[1]]$ageyoung, "int"),
            age_units = use_na(x$agerange[[1]]$units, "int"),
            notes = use_na(x$datasetnotes, "char"),
            pi_list = x$pi_list,
            samples = NULL,
            specimens = NULL
          )
          list(collectionunitid = cu$collectionunitid, dataset = do.call(build_dataset, ds))
        })
      })
    }
    needs_flattening <- all(purrr::map_lgl(datasets_, ~ is.list(.x) && all(purrr::map_lgl(.x, is.list))))
    if (needs_flattening) {
      datasets_ <- purrr::list_flatten(datasets_)
    }
    grouped <- split(datasets_, sapply(datasets_, function(x) x$collectionunitid))
    datasets <- purrr::imap(grouped, function(group, cu_id) {
      datasets_list <- purrr::map(group, "dataset")
      list(collectionunitid = as.integer(cu_id),
           datasets = new("datasets", datasets = datasets_list))
    }) %>% unname() 
    
    # Call for get_sites
    if ("collectionunits" %in% names(call)) {
      cu <- purrr::map(call$collectionunits, function(y){
        # Matched DSs
        matched_ds_entry <- purrr::keep(datasets, ~ .x$collectionunitid == y$collectionunitid)
        matched_ds <- if (length(matched_ds_entry) == 1) matched_ds_entry[[1]]$datasets else NULL
        cu1 <- list(collectionunitid = y$collectionunitid,
                    colldate = as.Date(testNull(y$colldate, NA)),
                    handle = use_na(y$handle, "char"),
                    datasets = matched_ds, #wait
                    chronologies = NULL,
                    location = use_na(y$location, "char"),
                    waterdepth = use_na(y$waterdepth, "int"),
                    gpslocation = use_na(y$gpslocation, "sf"),
                    collunittype = use_na(y$collunittype, "char"),
                    collectiondevice = use_na(y$collectiondevice, "char"),
                    collectionunitname = use_na(y$collectionunitname, "char"),
                    depositionalenvironment = use_na(y$depositionalenvironment, "char"),
                    defaultchronology = use_na(y$defaultchronology, "int"))
        do.call(build_collunits, cu1)
      })
      cu <- {
        ids <- sapply(cu, function(x) x$collectionunitid)
        cu[!duplicated(ids)]
      }
    } else {
      matched_ds <- datasets %>%
        keep(function(.x) .x$collectionunitid == call$collectionunitid) %>%
        pluck(1, "datasets")
      cu <- list(collectionunitid = call$collectionunitid,
                 colldate = as.Date(testNull(call$colldate, NA)),
                 handle = use_na(call$handle, "char"),
                 datasets = matched_ds, #wait
                 chronologies = NULL,
                 location = use_na(call$location, "char"),
                 waterdepth = use_na(call$waterdepth, "int"),
                 gpslocation = use_na(call$gpslocation, "sf"),
                 collunittype = use_na(call$collunittype, "char"),
                 collectiondevice = use_na(call$collectiondevice, "char"),
                 collectionunitname = use_na(call$collectionunitname, "char"),
                 depositionalenvironment = use_na(call$depositionalenvironment, "char"),
                 defaultchronology = use_na(call$defaultchronology, "int")
      )
      cu <- list(do.call(build_collunits, cu))
    }
    
    collunits <- new("collunits", collunits = cu)
    s <- list(
      sitename = call$sitename,
      siteid = call$siteid,
      geography = call$geography,
      altitude = call$altitude,
      description = call$sitedescription,
      notes = call$sitenotes,
      collunits = collunits
    )
    do.call(build_site, s)
  })
  
  sites <- new("sites", sites = new_sites)
  return(sites)
}
