#' @title get_sites
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @importFrom methods new
#' @description
#' Information for Fossil Sites
#' Look for a site details using only a site ID or for multiple sites using possible sitenames, max latitudes or min latitudes.
#' Displays a table with the following columns: siteid, sitename, lat, long, and elev.
#' The function takes parameters defined by the user and returns a list
#' of contact information supplied by the Neotoma Paleoecological Database.
#' The user may define all or none of the possible fields.  The function contains
#' data checks for each defined parameter.
#' @param x Use a single number to extract site information
#' @param ... accepted arguments: sitename, altmax, altmin
#' @return The function returns either a single item of class \code{"try-error"} describing
#'    the reason for failure (either mis-defined parameters or an error from the Neotoma API),
#'    or a table of sites, with rows corresponding to the number of individual
#'    sites returned by the Neotoma API.  
#'    Each "site" object contains 6 parameters that can be accessed as well:
#'    siteid, sitename, location, altitude, description, limited collection units information.
#' \item{ \code{siteid} }{site ID number}
#' \item{ \code{sitename} }{site's name}
#' \item{ \code{location} }{sf object that describes site's location}    
#' \item{ \code{description} }{}
#' \item{ \code{collunits} }{limited information on collunits}  
#' @examples \dontrun{
#' To find all sites with a min altitude of 12 and a max altitude of 25:
#' sites_12to25 <- get_sites(altmin=12, altmax=25)
#'
#' To find all sites that contain the string "Alex%"
#' alex.sites <- get_sites(sitename="Alex%")
#' 
#' To find all examples in Brazil
#' brazil <- '{"type": "Polygon", 
#' "coordinates": [[
#'  [-73.125, -9.102096738726443],
#'  [-56.953125,-33.137551192346145],
#'  [-36.5625,-7.710991655433217],
#'  [-68.203125,13.923403897723347],
#'  [-73.125,-9.102096738726443]
#' ]]}'
#' brazil_sites <- get_sites(loc = brazil[1])
#' }
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
    place <- st_read(result[2]$data[[i]]$geography, quiet = TRUE)
    if(is.na(result[2]$data[[i]]$altitude)){
      elev <- NA_integer_
    }else{
      elev <- result[2]$data[[i]]$altitude}
    siteid <- result[2]$data[[i]]$siteid
    sitename <- result[2]$data[[i]]$sitename
    description <- as.character(result[2]$data[[i]]$sitedescription)
    
    if(!(is.null(result[2]$data[[i]]$sitenotes))){
      sitenotes <- as.character(result[2]$data[[i]]$sitenotes)
    }else{
      sitenotes <- NA_character_
    }
    collunit <- map(result[2]$data[[i]]$collectionunits,
                    function(x) {
                      x <- new("collunit",
                               collunitid = x$collectionunitid,
                               collunitname = sitename,
                               colldate = as.Date(character(0)),
                               substrate = x$collectionunittype,
                               location = place,
                               handle = x$handle,
                               datasets = new('datasets',
                                              datasets = map(x$datasets, function(y) {
                                                ds = new('dataset',
                                                         datasetid = y$datasetid,
                                                         datasettype = y$datasettype,
                                                         datasetname = sitename,
                                                         location = place,
                                                         notes = NA_character_)
                                              })))
                      return(x)
                    })
    
    new_site <- new("site",
                    siteid = siteid,
                    sitename = sitename,
                    location = place, 
                    altitude = elev,
                    notes = sitenotes,
                    description = description,
                    collunits = new("collunits",
                                    collunits = collunit))
    sites <- append(sites, new_site)
    output <- new('sites', sites = sites)
  }
  return(output)
}

#' @title Get Site Information for Fossil Sites
#' @param ... accepted arguments: siteid, sitename, altmax, altmin, loc
#' @examples
#' get_sites(sitename = "Alexander Lake")
#' @export
get_sites.default <- function(..., verbose=0) {
  
  cl <- as.list(match.call())
  
  possible_arguments <- c("sitename", "altmax", "altmin", "loc")
  
  cl[[1]] <- NULL
  
  for(name in names(cl)){
    if(!(name %in% possible_arguments)){
      message(paste0(name, " is not an allowed argument. Argument will be ignored. Choose from the allowed arguments: sitename, altmax, altmin, loc"))
    }
  }
  
  cl <- lapply(cl, eval, envir = parent.frame())
  
  error_check <- check_args(cl)
  
  if (error_check[[2]]$flag == 1) {
    stop(paste0(unlist(error_check[[2]]$message), collapse = '\n  '))
  } else {
    cl <- error_check[[1]]
  }
  
  if("loc" %in% names(cl)){
    if(is.numeric(cl$loc)){
      coords <- cl$loc
      my_bbox <- sf::st_bbox(c(xmin = coords[1], xmax = coords[2], ymax = coords[3], ymin = coords[4]), crs = st_crs(4326))
      
      if(is.na(my_bbox$xmin)){
        stop("Numeric coordinates need to be an array of 4 units.")
      }
      
      if(is.na(my_bbox$xmax)){
        stop("Numeric coordinates need to be an array of 4 units.")
      }
      
      if(is.na(my_bbox$ymin)){
        stop("Numeric coordinates need to be an array of 4 units.")
      }
      
      if(is.na(my_bbox$ymax)){
        stop("Numeric coordinates need to be an array of 4 units.")
      }
      
      my_bbox <- st_as_sfc(my_bbox)
      new_geojson <- geojsonsf::sfc_geojson(my_bbox)
      new_geojson <- new_geojson[1]
      
      baseURL <- paste0('data/sites?loc=',new_geojson[1])
      for(name in names(cl)){
        if(!(name == "loc")){
          baseURL <- paste0(baseURL, "&", name, "=", paste0(cl[name]))
        }
      }
      result <- parseURL(baseURL) %>% 
        cleanNULL()
      
    }else{
      
      baseURL <- paste0('data/sites')
      result <- parseURL(baseURL, ...) %>% 
        cleanNULL()
    }
  }else{
    
    baseURL <- paste0('data/sites')
    
    result <- parseURL(baseURL, ...) %>%  cleanNULL()
  }
  
  if(is.null(result$data[1][[1]])){
    return(NULL)
  }else{
    output <- parse_site(result)
    
    result_length <- length(result[2]$data)
    
    if(verbose == 1){
      cat("A site object containing", result_length, "sites and 6 parameters. \n")
    }
    
    return(output)
  }
  
}

#' @title Get Site Information for Fossil Sites
#' @param siteid The numeric site ID from Neotoma
#' @export
get_sites.numeric <- function(x, ..., verbose =0) {
  useNA <- function(x, type){
    if (is.na(x)){
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    }else{
      return(x)
    }
  }
  
  if (length(x) > 0){
    siteids <- paste0(x, collapse = ',')
  }
  
  baseURL <- paste0('data/sites/', siteids)
  
  result <- parseURL(baseURL)
  
  result_length <- length(result[2]$data)
  
  if(result_length > 0){
    output <- parse_site(result)
    if(verbose == 1){
      cat("A site object containing", result_length, "sites and 6 parameters. \n")
    }
    return(output)
  }else{
    return(NULL)
  }
}