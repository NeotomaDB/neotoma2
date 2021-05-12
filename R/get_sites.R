#' @title Get Site
#' @import gtools
#' @details
#' Information for Fossil Sites
#' Look for a site details using only a site ID or for multiple sites using possible sitenames, max latitudes or min latitudes.
#' Displays a table with the following columns siteid, sitename, lat, long, and elev.
#' Returns an object "sites" that contains multiple single "site" objects. Each of this objects has 6 parameters that can be accessed as well.
#' @param siteid integer A site ID
#' @param sitename The site's name
#' @param altmax The coordinates to create an sf object
#' @param altmin The coordinates to create an sf object
#' @export
#' @examples
#' get_sites(24)
#' 
#' A site object containing 1 sites and 6 parameters. 
#' | siteid  | sitename       |    lat    |   long   | elev |
#' | :------ |:--------------:|:---------:|:--------:|-----:|
#' | 24      | Alexander Lake | -60.58333 | 53.33333 |  143 |
#' 
#' alexander <- get_sites(sitename="Alexander Lake")
#' alexander
#' 
#' A site object containing 1 sites and 6 parameters. 
#' | siteid  | sitename       |    lat    |   long   | elev |
#' | :------ |:--------------:|:---------:|:--------:|-----:|
#' | 24      | Alexander Lake | -60.58333 | 53.33333 |  143 |
#' 
#' alexander@sites[[1]]@siteid
#' [1] 24
#' @md
get_sites <- function(siteid = NA, ...) {
  if(!missing(siteid)) {
    UseMethod('get_sites', siteid)
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
                                                return(ds)
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

#' @title Get Site Numeric
#' @import lubridate
#' @importFrom methods new
#' @param x Use a single number to extract site information
#' @examples
#' get_sites(24)
#' 
#' A site object containing 1 sites and 6 parameters. 
#' | siteid  | sitename       |    lat    |   long   | elev |
#' | :------ |:--------------:|:---------:|:--------:|-----:|
#' | 24      | Alexander Lake | -60.58333 | 53.33333 |  143 |
#' @md
get_sites.numeric <- function(siteid, ...) {
  
  useNA <- function(siteid, type) {
    if (is.na(siteid)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(siteid)
    }
  }
  
  if (length(siteid) > 0) {
    sitename <- paste0(siteid, collapse = ',')
  }
  
  baseURL <- paste0('data/sites/', siteid)
  
  result <- parseURL(baseURL)
  
  output <- parse_site(result)
  
  result_length <- length(result[2]$data)
  
  cat("A site object containing", result_length, "sites and 6 parameters. \n")
  
  return(output)

}


#' @title Get Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @param sitename The site's name
#' @param altmax The coordinates to create an sf object
#' @param altmin The coordinates to create an sf object
#' @examples
#' get_sites(sitename = "Alexander Lake")
#' 
#' A site object containing 1 sites and 6 parameters. 
#' | siteid  | sitename       |    lat    |   long   | elev |
#' | :------ |:--------------:|:---------:|:--------:|-----:|
#' | 24      | Alexander Lake | -60.58333 | 53.33333 |  143 |
#' @md

#' @export
get_sites.default <- function(...) {
  
  check_args(...)
  
  baseURL <- paste0('data/sites')
  result <- parseURL(baseURL, ...) %>% 
    cleanNULL()
 
  if(is.null(result$data[1][[1]])){
    output <- cat("I can't find a site for you. Are you using the right spelling? \n\n")
    return(output)
  }else{
    output <- parse_site(result)
    args <- list(...)
    
    result_length <- length(result[2]$data)
    
    cat("A site object containing", result_length, "sites and 6 parameters. \n")

    return(output)
  }

}

#' @title Check Arguments
#' @param arguments in ... form
check_args <- function(...) {
  args <- match.call()
  length(args)
  arg_names <- c()
  for(i in 2:length(names(args))){
    arg_names <- c(arg_names, names(args[i]))
  }
  
  if(length(arg_names)==0){
    stop("You need to use at least 1 of the following arguments: sitename, altmax, altmin")
  }else{
    for(i in 1:length(arg_names)){
      if(!(names(args)[i] %in% c("", 'sitename', 'altmax', 'altmin'))){
        stop("Are you using the following arguments: sitename, altmax, altmin ?")
      }
    }
    
    if(("sitename" %in% names(args))|("altmax" %in% names(args)) | ("altmin" %in% names(args))){
      if(("sitename" %in% names(args))){
        
          if(class(args$sitename) != 'character'){
            stop("Sitename should be a character")
          }
      }
      
      if(("altmin" %in% names(args))){
          if(class(args$altmin) != 'numeric'){
            stop("Altmin should be a number")
          }
      }
      
      if(("altmax" %in% names(args))){
          if(class(args$altmax) != 'numeric'){
            stop("Altmax should be a number")
          }
      }
      
      if(("altmax" %in% names(args)) & ("altmin" %in% names(args))){
        if(args$altmax<args$altmin){
          stop("altmax cannot be smaller than altmin")
        }
      }
      
      
    }
  }
}