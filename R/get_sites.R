#' @title Get Site Information for Fossil Sites
#' @import gtools
#' @param x integer A contact ID
#' @param contactname A full or partial name for an individual contributor to the database.
#' @param familyname The full or partial last name for an individual contributor to the database.
#' @export
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
  
  return(output)
  
}


#' @title Get Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @param sitename The site's name
#' @param altmax The coordinates to create an sf object
#' @param altmin The coordinates to create an sf object
#' @export
get_sites.default <- function(...) {
  
  check_args(...)
  
  baseURL <- paste0('data/sites')
  result <- parseURL(baseURL, ...) %>% 
    cleanNULL()
 
  if(is.null(result$data[1][[1]])){
    output <- cat("I can't find a site for you. Are you using the right spelling? \n")
    return(output)
  }else{
    output <- parse_site(result)
    args <- list(...)
    print(paste0(names(args)))
    return(output)
  }

}

#' @title Check Arguments
#' @param arguments in ... form
check_args <- function(...) {
  args <- match.call()
  
  arg_names <- c()
  names_perms <- c()
  for(i in 2:length(names(args))){
    arg_names <- c(arg_names, names(args[i]))
    arg_names <- mixedsort(arg_names)
  }
  
  for(i in 1:length(arg_names)){
    permutation <- permutations(n=length(arg_names),r=i,v=arg_names,repeats.allowed=F)
    for(j in 1:length(arg_names)){
      permutation <- permutations(n=length(arg_names),r=i,v=arg_names,repeats.allowed=F)[j,]
      names_perms <- c(names_perms, c(permutation))
    }
    
    
  }
  
  print(names(args))
  for(i in 1:length(arg_names)){
    if(!(names(args)[i] %in% c("", 'sitename', 'altmax', 'altmin'))){
      stop("Are you using one of the following arguments: sitename, altmax, altmin ?")
    }
  }
  
  print(names_perms)
  
  if(("sitename" %in% names(args))|("altmax" %in% names(args)) | ("altmin" %in% names(args))){
    cat("let's try this")

    if("sitename" == names(args)[2]){
      if(class(args[[2]]) == 'character'){
        cat("we're good")
      }else{
        cat("wrong dt")
      }
      
    print(names(args)[2])
    print(args[[2]])
    
    }else{
      print("not the right data type")
    }
    
    
    # some other code and operations
    # that use the ellipsis-arguments as “native” variables…

  }else{
    stop("Use 1 or more of the following arguments: sitename, altmax, altmin")
  }
}