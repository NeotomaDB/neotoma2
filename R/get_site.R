#' @title Get Site Information for Fossil Sites
#' @param x integer A contact ID
#' @param contactname A full or partial name for an individual contributor to the database.
#' @param familyname The full or partial last name for an individual contributor to the database.
#' @export

get_site <- function(x = NA, ...) {
    UseMethod('get_site')
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
    location <- st_read(result[2]$data[[i]]$geography, quiet = TRUE)
    siteid <- result[2]$data[[i]]$siteid
    sitename <- result[2]$data[[i]]$sitename
    description <- as.character(result[2]$data[[i]]$sitedescription)
    notes <- NA_character_
   # cat("Sitename", sitename, "\n")
  #  cat("Siteid", siteid, "\n")
   # cat("Location:")
  #  print(location)
    
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

    output <- new("site",
                  siteid = siteid,
                  sitename = sitename,
                  location = location,      
                  description = description,
                  notes = NA_character_,
                  collunits = new("collunits",
                                  collunits = collunit))
  
    
    sites <- append(sites, output)
    }
  
  
  
  return(sites)
  
}

#' @title Get Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @export
get_site.numeric <- function(x, ...) {
  
  useNA <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
      cat("x is na", x)
    } else {
      cat("x is not na", x)
      return(x)
    }
  }
  
  if (length(x) > 0) {
    sitename <- paste0(x, collapse = ',')
  }
  
  baseURL <- paste0('data/sites/', x)
  
  result <- parseURL(baseURL)
  
  location <- st_read(result$data[[1]]$geography, quiet = TRUE)
  
  collunit <- map(result$data[[1]]$collectionunits,
                  function(x) {
                    x <- new("collunit",
                             collunitid = x$collectionunitid,
                             colldate = NA_Date_,
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
  
  output <- new("site", siteid = result$data[[1]]$siteid,
                sitename = result$data[[1]]$sitename,
                location = location,
                description = useNA(result$data[[1]]$sitedescription, "char"),
                notes = NA_character_,
                collunits = new("collunits",
                                collunits = collunit))
  
  output <- new('sites', sites = list(output))
  
  return(output)
  
}

#' @title Get Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @param sitename gets sitename
#' @export
get_site.default <- function(sitename = NA, lat = NA, ...) {
  
  useNA <- function(sitename, type) {
    if (is.na(sitename)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
      cat("x is na", sitename)
    } else {
      cat("x is not na", sitename)
      return(sitename)
    }
  }
  
  if (length(sitename) > 0) {
    sitename <- paste0(sitename, collapse = ',')
  }
  
  if (length(lat) > 0) {
    lat <- paste0(lat, collapse = ',')
  }
  
  cl <- as.list(match.call())
  print(cl)
  
  
  x <- gsub(" ", "%20", sitename)
  
  # Add if for sitename, if sitename exists, append
  baseURL <- paste0('data/sites?sitename=', x)
  
  
  
  # Add if for lat, if lat exists, append - first make sure sitename works
  
  
  result <- parseURL(baseURL)
  
  
  location <- st_read(result$data[[1]]$geography, quiet = TRUE)
  
  collunit <- map(result$data[[1]]$collectionunits,
                  function(x) {
                    x <- new("collunit",
                             collunitid = x$collectionunitid,
                             colldate = NA_Date_,
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
  
  output <- new("site", siteid = result$data[[1]]$siteid,
                sitename = result$data[[1]]$sitename,
                location = location,
                description = useNA(result$data[[1]]$sitedescription, "char"),
                notes = NA_character_,
                collunits = new("collunits",
                                collunits = collunit))
  
  output <- new('sites', sites = list(output))
 
  
  return(output)
}