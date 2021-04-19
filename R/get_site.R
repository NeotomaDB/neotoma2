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
  
  location <- st_read(result$geography, quiet = TRUE)
  
  collunit <- map(result$collectionunits,
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
  
  output <- new("site", siteid = result$siteid,
                sitename = result$sitename,
                location = location,
                description = as.character(result$sitedescription),
                notes = NA_character_,
                collunits = new("collunits",
                                collunits = collunit))
  
  return(output)
  
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
      cat("hola", x)
    } else {
      cat("adios", x)
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
#' @export
get_site.default <- function(...) {
  
  cat("I am in default")
  g = st_sfc(st_point(1:2))
  
  site <- new("site",
              siteid = 1,
              sitename = "New",
              location = st_sf(a=3,g),
              description = "character",
              notes = "character",
              collunits= "collunits")
  
  
  return(site)
}