#' @title Get Site Information for Fossil Sites
#' @param x integer A contact ID
#' @param contactname A full or partial name for an individual contributor to the database.
#' @param familyname The full or partial last name for an individual contributor to the database.
#' @export
get_site <- function(x = NA, ...) {
  if(!missing(x)) {
    UseMethod('get_site', x)
  } else {
    UseMethod('get_site', NA)
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
  cat("This is the length:", result_length, "\n")
  
  sites <- c()
  for(i in 1:result_length) {
    # i-th element result[2]$data[[i]]$
    place <- st_read(result[2]$data[[i]]$geography, quiet = TRUE)
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
#' @export
get_site.numeric <- function(siteid, ...) {
  
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
  
  return(output)
  
}

#' @title Get Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @param sitename The site's name
#' @param altmax The coordinates to create an sf object
#' @param altmin The coordinates to create an sf object
#' @export
get_site.default <- function(sitename = NA, altmax = NA, altmin = NA) {
  
  useNA <- function(sitename, type) {
    if (is.na(sitename)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(sitename)
    }
  }
  
  if (length(sitename) > 0) {
    sitename <- paste0(sitename, collapse = ',')
  }
  
  if (length(altmax) > 0) {
    altmax <- paste0(altmax, collapse = ',')
  }
  
  cl <- as.list(match.call())
  print(cl)
  
  
  x <- gsub(" ", "%20", sitename)

  if(sitename == 'NA'){
    baseURL <- paste0('data/sites?')
    if(altmax == 'NA'){
      baseURL 
      if(is.na(altmin)){
        cat("You need to pass some arguments")
      }else{
        baseURL <- paste0(baseURL, 'altmin=', altmin)
      }
    }else{
      baseURL <- paste0(baseURL, 'altmax=', altmax)
      if(is.na(altmin)){
        print("This is url")
      }else{
        baseURL <- paste0(baseURL, 'altmin=', altmin)
      }
    }
  }else{
    baseURL <- paste0('data/sites?sitename=', x)
    cat("entering sitename is given")
    if(altmax == 'NA'){
      cat("entering altmax is not given")
      baseURL
      if(is.na(altmin)){
        print("This is url")
      }else{
        baseURL <- paste0(baseURL, 'altmin=', altmin)
      }
    }else{
      baseURL <- paste0(baseURL, '&altmax=', altmax)
      if(is.na(altmin)){
        print("This is url")
      }else{
        baseURL <- paste0(baseURL, 'altmin=', altmin)
      }
    }
  }
  
  print(baseURL)
  
  result <- parseURL(baseURL)
  
  if(is.null(result$data[1][[1]])){
    output <- cat("I can't find a site for you. Are you using the right spelling? \n")
    return(output)
  }else{
    output <- parse_site(result)
    return(output)
  }

}