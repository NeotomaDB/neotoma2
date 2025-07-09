#' @title get_datasets
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @import purrr
#' @import gtools
#' @import lubridate
#' @import geojsonsf
#' @importFrom methods new
#' @export
get_datasets <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_datasets", x)
  } else {
    UseMethod("get_datasets", NA)
  }
}

parse_dataset <- function(result) {
  data <- result$data
  new_sites <- purrr::map(data, function(x) {
    if (is.null(x$sites)) {
      call <- x$site
    } else {
      call <- x$sites$site
    }
    # DSs
    if (is.null(x$sites$datasets)) {
      ds_ <- x$site$datasets
    } else {
      ds_ <- x$sites$datasets
    }
    
    datasets <- purrr::map(ds_, function(x){
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
      do.call(build_dataset, ds)
    })
    
    datasets <- new("datasets", datasets = datasets)
    
    # A DS exists only in one CU
    # This is different when getting sites
    cu <- list(collectionunitid = call$collectionunitid,
               colldate = as.Date(testNull(call$colldate, NA)),
               handle = call$handle,
               datasets = datasets, #wait
               chronologies = NULL
               )
    cu <- do.call(build_collunits, cu)
    collunits <- new("collunits", collunits = list(cu))
    
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

#' @title Get Dataset Numeric
#' @param x Use a single number to extract site information
#' @param ... Additional parameters to get_datasets
#' @returns `sites` object with full metadata up to the `dataset` level
#' @examples \donttest{
#' allds <- get_datasets(1:3)
#' }
#' @export
get_datasets.numeric <- function(x, ...) {
  if (length(x) > 0) {
    dataset <- paste0(x, collapse = ",")
  }
  base_url <- paste0("data/datasets/", dataset)
  result <- parseURL(base_url, ...)
  if (length(result[2]$data) > 0) {
    output <- parse_dataset(result)
    return(output)
  } else {
    return(NULL)
  }
}

#' @title Get Dataset Default
#' @param x Use a single number to extract dataset information
#' @param ... accepted arguments, see details for more information.
#' @importFrom utils URLencode
#' @returns `sites` object with full metadata up to the `dataset` level

get_datasets.default <- function(x, ...) {
  params <- get_params("datasets")
  oo <- options(scipen = 9999999)
  on.exit(options(oo))
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  
  cl <- lapply(cl, eval, envir = parent.frame())
  all_data <- ifelse(is.null(cl$all_data), FALSE, TRUE)
  
  # Location geojson / coords array
  if ("loc" %in% names(cl)) {
    loc <- parse_location(cl$loc)
    base_url <- paste0("data/datasets?loc=", URLencode(loc, reserved = TRUE))
    if(length(base_url)>1){
      stop("Multiple polygons cannot be handled, pass one polygon at a time.")
    }
    
    for (name in names(cl)) {
      if (!(name == "loc")) {
        if (!(name == "all_data")) {
          base_url <- paste0(base_url, "&", name, "=", paste0(cl[name]))
        }
      }
    }

    # loc and all_data present
    if ("all_data" %in% names(cl)){
      result <- parseURL(base_url, all_data = cl$all_data)
    } else {
      result <- parseURL(base_url)
    }
  } else {
    base_url <- paste0("data/datasets")
    result <- parseURL(base_url, ...) 
  }
  
  if (is.null(result$data[1][[1]]) || is.null(result[1][[1]])) {
    return(NULL)
    
  } else {
    output <- parse_dataset(result)
    return(output)
  }
}
