#' @title Get Neotoma site metadata
#' @import gtools
#' @details
#' Information for Fossil Sites
#' Look for a site details using only a site ID or for multiple sites using possible sitenames, max latitudes or min latitudes.
#' Displays a table with the following columns siteid, sitename, lat, long, and elev.
#' Returns an object "sites" that contains multiple single "site" objects. Each of this objects has 6 parameters that can be accessed as well.
#' @param x Use a single number to extract site information
#' @param ... accepted arguments: sitename, altmax (The coordinates to create an sf object), altmin (The coordinates to create an sf object)
#' @examples
#' get_sites(24)
#' get_sites(altmin=12, altmax=15)
#' @export
get_sites <- function(x=NA, ...) {
  if(!missing(x)) {
    UseMethod('get_sites', x)
  } else {
    UseMethod('get_sites', NA)
  }
}

parse_site <- function(result) {
  
  fixNull <- function(x) {
    for (i in 1:length(x)) {
      if (is.null(x[[i]])) {
        x[[i]] <- NA
      } else {
        if (class(x[[i]]) == 'list') {
          x[[i]] <- fixNull(x[[i]])
        }
      }
    }
    return(x)
  }
  
  result <- result %>% fixNull()
  result_length <- length(result[2]$data)
  
  sites <- c()
  
  for(i in 1:result_length) {
    # i-th element result[2]$data[[i]]$
    place <- st_read(result[2]$data[[i]]$geography, quiet = TRUE)
    elev <- result[2]$data[[i]]$altitude
    siteid <- result[2]$data[[i]]$siteid
    sitename <- result[2]$data[[i]]$sitename
    description <- as.character(result[2]$data[[i]]$sitedescription)
    notes <- NA_character_
    
    collunit <- map(result[2]$data[[i]]$collectionunits,
                    function(x) {
                      x <- new("collunit",
                               collunitid = x$collectionunitid,
                               colldate = as.Date("2007-02-01"),
                               handle = x$handle,
                               datasets = new('datasets',
                                              datasets = map(x$datasets, function(y) {
                                                ds = new('dataset',
                                                         datasetid = y$datasetid,
                                                         datasettype = y$datasettype,
                                                         datasetname = NA_character_,
                                                         notes = NA_character_)
                                              })))
                      return(x)
                    })
    
    new_site <- new("site",
                  siteid = siteid,
                  sitename = sitename,
                  location = place, 
                  altitude = elev,
                  description = description,
                  notes = NA_character_,
                  collunits = new("collunits",
                                  collunits = collunit))
    
    
    sites <- append(sites, new_site)
    
    output <- new('sites', sites = sites)
  }
  
  
  return(output)
}

#' @title Get Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @param ... accepted arguments: siteid (site identifier from Neotoma), sitename, altmax (The coordinates to create an sf object), altmin (The coordinates to create an sf object)
#' @examples
#' get_sites(sitename = "Alexander Lake")
#' @export
get_sites.default <- function(...) {
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  
  error_check <- check_args(cl)
  
  if (error_check[[2]]$flag == 1) {
    stop(paste0(unlist(error_check[[2]]$message), collapse = '\n  '))
  } else {
    cl <- error_check[[1]]
  }
  
  baseURL <- paste0('data/sites')
  result <- parseURL(baseURL, ...) %>% 
    cleanNULL()
  
  if(is.null(result$data[1][[1]])){
    warning('I cannot find a site for you. Are you using the right spelling? \n')
    return(NULL)
  }else{
    output <- parse_site(result)
    
    result_length <- length(result[2]$data)
    
    cat("A site object containing", result_length, "sites and 6 parameters. \n")
    
    return(output)
  }
  
}

#' @title Get Site from a site ID
#' @import lubridate
#' @importFrom methods new
#' @param siteid The numeric site ID from Neotoma
#' @param ... arguments in ellipse form
#' @examples
#' get_sites(siteid = 24)
#' @export
get_sites.numeric <- function(x, ...) {
  
  useNA <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }
  
  if (length(x) > 0) {
    siteids <- paste0(x, collapse = ',')
  }
  
  baseURL <- paste0('data/sites/', siteids)
  
  result <- parseURL(baseURL)
  
  output <- parse_site(result)
  
  result_length <- length(result[2]$data)
  
  cat("A site object containing", result_length, "sites and 6 parameters. \n")
  
  return(output)

}
