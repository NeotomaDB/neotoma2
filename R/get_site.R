#' @title Get Site Information for Fossil Sites
#' @param x integer A contact ID
#' @param contactname A full or partial name for an individual contributor to the database.
#' @param familyname The full or partial last name for an individual contributor to the database.
#' @export
get_site <- function(siteid = NA, ...) {
  if(!missing(siteid)) {
    UseMethod('get_site', siteid)
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
get_site.default <- function(...) {
  
  baseURL <- paste0('data/sites')
  result <- parseURL(baseURL, ...) %>% 
    cleanNULL()
  
  if(is.null(result$data[1][[1]])){
    output <- cat("I can't find a site for you. Are you using the right spelling? \n")
    return(output)
  }else{
    output <- parse_site(result)
    print(paste0(...))
    tracer = match.call()
    tracer
    return(output)
  }

}

#' @title Check Arguments
#' @import lubridate
#' @param arguments get_site arguments
check_args.default <- function(...) {}
