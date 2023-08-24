#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @import jsonlite
#' @importFrom methods new
#' @description
#' Information for Fossil Datasets
#' @param x Use a single number to extract site information
#' @param verbose Status bar of items being downloaded
#' @param ... accepted arguments: sites, datasets
#' @details
#' The `get_downloads()` command wraps the Neotoma API
#' ([api.neotomadb.org](https://api.neotomadb.org)) call for `downloads`.
#' The call itself uses a SQL query which accepts any one of the following
#' parameters:
#'  * `datasetid` The unique dataset ID (integer) in Neotoma. Can be passed
#' as a vector of dataset IDs.
#'  * `all_data` The API only downloads the first 25 records of the query. 
#'  For the complete records, use `all_data=TRUE`
#' @returns The function returns either a single item of class
#' \code{"try-error"} describing the reason for failure
#' (either misdefined parameters or an error from the Neotoma API),
#' or a table of sites, with rows corresponding to the number of
#' individual sites and datasets returned by the Neotoma API.
#' Each "site" object contains 6 parameters that can be accessed as well:
#' \item{ \code{siteid} }{site ID number}
#' \item{ \code{sitename} }{site's name}
#' \item{ \code{location} }{sf object that describes site's location}
#' \item{ \code{description} }{}
#' \item{ \code{collunits} }{limited information on collunits}
#' Each "collection unit" embedded in the "sites" object contains
#' 6 parameters that can be accessed as well:
#' \item{ \code{collunitid}}{collection unit ID number}
#' \item{ \code{handle} }{collection unit's handle}
#' \item{ \code{collunitname} }{collection unit's name}
#' \item{ \code{colldate} }{date in collection unit}
#' \item{ \code{substrate} }{substrate}
#' \item{ \code{location} }{sf object that describes site's location}
#' \item{ \code{datasets} }{detailed information regarding dataset}
#' Each "dataset" nested in the "collection unit" contains the
#' following detail of information:
#' \item{ \code{datasetid} }{dataset ID number}
#' \item{ \code{datasetname} }{site's name}
#' \item{ \code{datasettype} }{type of data found}
#' \item{ \code{location} }{sf object that describes site's location}
#' \item{ \code{notes} }{notes on the dataset}
#' \item{ \code{taxa table} }{taxa table}
#' \item{ \code{pi list} }{P.I. info}
#' \item{ \code{analyst} }{analyst info}
#' \item{ \code{metadata} }{dataset metadata}
#' @examples \donttest{
#' # To find the downloads object of dataset 24:
#' downloads24 <- get_downloads(24)
#'
#' # To find all downloads in Brazil
#' brazil <- '{"type": "Polygon",
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]]]}'
#' brazil_datasets <- get_datasets(loc = brazil[1])
#' brazil_downloads <- get_downloads(brazil_datasets)
#' }
#' @export
get_downloads <- function(x = NA, verbose = TRUE, ...) {
  if (!missing(x)) {
    UseMethod("get_downloads", x)
  }else {
    UseMethod("get_downloads", NA)
  }
}

parse_download <- function(result, verbose = TRUE) {
  dls <- result$data %>%
    cleanNULL()
  
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
  
  for (i in 1:length(dls)) {
    if (length(my_sites_list) == 0) {
      my_site <- build_sites(dls[[i]])
      my_sites_list <- c(my_sites_list, my_site)
      
    } else {
      ids <- getids(my_sites_list, order = FALSE)
      matches <- check_match(dl_index[i,], ids)
      
      if (max(matches) == 0) {
        # We're adding a site:
        my_site <- build_sites(dls[[i]])
        my_sites_list <- c(my_sites_list, my_site)
        # for some reason, the 19 sites where added fine
        
      } else if (max(matches) == 1) {
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
  }
  return(my_sites_list)
}

#' @title get_downloads
#' @param x Use a single number to extract site information
#' @param verbose Should text be printed during the download process?
#' @param ... arguments in ellipse form
#' @returns The function returns either a single item of class
#' \code{"try-error"} describing the reason for failure
#' (either misdefined parameters or an error from the Neotoma API),
#' or a table of sites, with rows corresponding to the number of
#' individual sites and datasets returned by the Neotoma API.
#' @export
get_downloads.numeric <- function(x, verbose = TRUE, ...) {
  
  if (length(x) > 0) {
    dataset <- paste0(x, collapse = ",")
  }

  base_url <- paste0("data/downloads?datasetid=", dataset)
  result <- parseURL(base_url, ...) # nolint
  
  output <- parse_download(result, verbose = verbose)
  
  return(output)
}

#' @title get_downloads sites
#' @param x sites object
#' @param verbose Should text be printed during the download process?
#' @param ... arguments in ellipse form
#' @importFrom stats na.omit
#' @returns The function returns either a single item of class
#' \code{"try-error"} describing the reason for failure
#' (either misdefined parameters or an error from the Neotoma API),
#' or a table of sites, with rows corresponding to the number of
#' individual sites and datasets returned by the Neotoma API.
#' @export
get_downloads.sites <- function(x, verbose = TRUE, ...) {
  
  output <- getids(x) %>% 
    dplyr::filter(!is.na(suppressWarnings(as.numeric(siteid))),
                  !is.na(suppressWarnings(as.numeric(datasetid))))
  
  ids2 <- getids(x) %>% dplyr::filter(is.na(suppressWarnings(as.numeric(siteid))) |
                                        is.na(suppressWarnings(as.numeric(datasetid))))
  
  if(nrow(ids2)!=0){
    warnsite <- sprintf("SiteID %s or DatasetID %s does not exist in the Neotoma DB yet or it has been removed.
                        It will be removed from your search.",  paste0(ids2$siteid,collapse = ", "), paste0(ids2$datasetid,collapse = ", "))
    warning(warnsite)
  }
  
  output <- output %>%
    dplyr::select(datasetid) %>%
    stats::na.omit() %>%
    unique() %>%
    unlist() %>%
    as.numeric() %>%
    suppressWarnings()
  
  ## Fixing all data
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  
  if('all_data' %in% names(cl)){
    all_data = cl$all_data
  }else{
    cl[['all_data']] = TRUE
  }
  
  if('limit' %in% names(cl)){
    cl[['all_data']] = FALSE
  }
  
  if('offset' %in% names(cl)){
    cl[['all_data']] = FALSE
  }
  ## Fixing all data line
  
  cl[['x']] <- output
  cl[['verbose']] <- verbose
  
  output <- do.call(get_downloads, cl)

  return(output)
}

#' @title get_downloads JSON
#' @param x sites object
#' @param verbose Should text be printed during the download process?
#' @param ... arguments in ellipse form
#' @returns The function returns either a single item of class
#' \code{"try-error"} describing the reason for failure
#' (either misdefined parameters or an error from the Neotoma API),
#' or a table of sites, with rows corresponding to the number of
#' individual sites and datasets returned by the Neotoma API.
#' @importFrom stats na.omit
#' @export
get_downloads.character <- function(x, verbose = TRUE, ...) {
  
  result <- jsonlite::fromJSON(x,
                               flatten = FALSE,
                               simplifyVector = FALSE)
  result <- result %>%
    cleanNULL()
  
  output <- parse_download(result, verbose = verbose)
  
  return(output)
}
