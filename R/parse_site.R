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
parse_download <- function(sites, dls, verbose = TRUE) {
  dl_index <- purrr::map(dls, function(x) {
    data.frame(siteid = x$site$siteid,
               collunitid = x$site$collectionunit$collectionunitid,
               datasetid = x$site$collectionunit$dataset$datasetid )}) %>%
    dplyr::bind_rows()
  
  my_sites_list <- c()
  siteids <- c()
  
  check_match <- function(dl_row, ids) {
    apply(ids, 1, function(x) sum(dl_row == x))
  }
  
  ids <- getids(sites, order = FALSE)
  for (i in 1:length(dls)) {
    matches <- check_match(dl_index[i,], ids)
    if (max(matches) == 1) {
      # We're adding a collection unit somewhere:
      st <- match(ids$siteid[which.max(matches)], unique(ids$siteid))
      
      newcu <- build_collunits(dls[[i]]$site$collectionunit)
      oldcu <- my_sites_list[[st]]@collunits@collunits
      
      my_sites_list[[st]]@collunits@collunits <- c(oldcu, newcu)
      
    } else if (max(matches) == 2) {
      # We're adding a dataset to an existing collection unit:
      
      st <- match(ids$siteid[which.max(matches)], unique(ids$siteid))
      
      cuids <- ids %>%
        dplyr::filter(siteid == unique(ids$siteid)[st], .preserve = TRUE)
      
      cuid <- which(unique(cuids$collunitid) == dl_index$collunitid[i])
      
      collunit <- my_sites_list[[st]]@collunits@collunits[[cuid]]
      newds <- build_dataset(dls[[i]]$site$collectionunit$dataset)
      collunit@datasets@datasets <- c(collunit@datasets@datasets,
                                      newds)
      my_sites_list[[st]]@collunits@collunits[[cuid]] <- collunit
    }
  }
  if (verbose) {
    cat(".")
  }
  return(my_sites_list)
}

parse_site <- function(result, parse_download = FALSE) {
  data <- result$data
  new_sites <- purrr::map(data, function(x) {
    if (!is.null(x$site)) {
      # get_datasets & get_downloads
      call <- x$site
    } else {
      # get_sites
      call <- x
    }
    # DSs Call
    # get_sites does not have one since I need to access the list of CUs
    if (!is.null(x$site$datasets)){
      # get_datasets
      ds_ <- x$site$datasets # list of lists
    } else if (!is.null(x$site$dataset)){
      # get_downloads
      print("get_downloads")
      ds_ <- x$site$dataset # simple list
    }  
    
    if (!is.null(call$collectionunit) && all(sapply(call$collectionunit, is.list))) {
      # get_sites
      datasets_ <- purrr::map(call$collectionunit, function(cu) {
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
    } else if (!is.null(x$site$datasets)){
      # get_datasets 
      # If API is corrected, this should also be the call for get_downloads
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
      # get_downloads - needed as API does not aggregate datasets in downloads call.
      if (is.null(ds_$agerange) || length(ds_$agerange) == 0){
        x$agerange <- list(list(ageold = NA, ageyoung = NA, units = NA))
      }
      ds <- list(
        datasetid = ds_$datasetid,
        database = use_na(ds_$database, "char"),
        doi = x$doi,
        datasettype = use_na(ds_$datasettype, "char"),
        datasetname = use_na(ds_$datasetname, "char"),
        age_range_old = use_na(ds_$agerange[[1]]$ageold, "int"),
        age_range_young = use_na(ds_$agerange[[1]]$ageyoung, "int"),
        age_units = use_na(ds_$agerange[[1]]$units, "int"),
        notes = use_na(ds_$datasetnotes, "char"),
        pi_list = ds_$pi_list,
        samples = NULL,
        specimens = NULL
      )
      cuid <- call$collectionunit$collectionunitid
      datasets_ <- list(list(collectionunitid = cuid, dataset = do.call(build_dataset, ds)))
      # Since I work with only one element at a time, I need a double list here. 
      # get_sites and get_datasets yield lists of lists
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
    if (!is.null(call$collectionunit) && all(sapply(call$collectionunit, is.list))){
      # Get Sites
      print('get_sites call')
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
      # Get datasets & hopefully get_downloads
      print('get_datasets call')
      if (!is.null(x$site$datasets)) {
        # This would be get_datasets
        call_ <- call
      } else {
        # This would be get_downloads
        call_ <- call$collectionunit
      }
      matched_ds <- datasets %>%
        purrr::keep(function(.x) .x$collectionunitid == call$collectionunitid) %>%
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
  
  if (parse_download){
    sites <- parse_download(sites, data, verbose = TRUE)
  }
  return(sites)
}
