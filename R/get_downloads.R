#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Information for Fossil Datasets
#' @param x Use a single number to extract site information
#' @param ... accepted arguments: sites, datasets
#' @return The function returns either a single item of class
#' \code{"try-error"} describing the reason for failure
#' (either mis-defined parameters or an error from the Neotoma API),
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
#' @examples \dontrun{
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
      } else if (max(matches) == 1) {
        # We're adding a collection unit somewhere:
        st <- ids %>% 
          mutate(match = matches) %>% 
          group_by(siteid) %>% 
          summarise(match = max(match)) %>% 
          select(match) %>% unlist() %>% 
          which.max()
        
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
      cat('.')
    }
  }
  return(my_sites_list)
}

#' @title get_downloads
#' @param x Use a single number to extract site information
#' @param ... arguments in ellipse form
#' @export
get_downloads.numeric <- function(x, verbose = TRUE, ...) {
  
  use_na <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }

  if (length(x) > 0) {
    dataset <- paste0(x, collapse = ",")
  }
  
  base_url <- paste0("data/downloads/", dataset)
  result <- parseURL(base_url) # nolint
  
  output <- parse_download(result)
  
  return(output)
}

#' @title get_downloads sites
#' @param x sites object
#' @param ... arguments in ellipse form
#' @export
get_downloads.sites <- function(x, ...) {
  
  use_na <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }
  
  output <- getids(x) %>%
    dplyr::select(datasetid) %>% 
    na.omit() %>%
    unique() %>% 
    unlist() %>% 
    get_downloads(x = ., verbose, ...)
  
  return(output)
}