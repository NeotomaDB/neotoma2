#' @title Get datasets - Data Sets Information for Fossil Sites
#' @import gtools
#' @param datasetid integer An internal Neotoma dataset identifier
#' @param ... arguments in ellipse form
#' @export
get_datasets <- function(datasetid = NA, ...) {
  if(!missing(datasetid)) {
    UseMethod('get_datasets', datasetid)
  } else {
    UseMethod('get_datasets', NA)
  }
}

parse_dataset <- function(result) {
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
    coll_units <- c()
    dataset_list <- c()
    
    # Sites 
    # Sitename
    if(is.null(result[2]$data[[i]]$site$sitename)){
      sitename <- result[2]$data[[i]]$sites$site$sitename
    }else{
      sitename <- result[2]$data[[i]]$site$sitename
    }
    
    # Site ID
    if(is.null(result[2]$data[[i]]$site$siteid)){
      siteid <- result[2]$data[[i]]$sites$site$siteid
    }else{
      siteid <- result[2]$data[[i]]$site$siteid
    }
    
    # Location
    location <- result[2]$data[[i]]$site$geography
    if(is.null(location)){
      location <- st_read(result[2]$data[[i]]$sites$site$geography, quiet = TRUE)
    }else{
      location <- st_read(result[2]$data[[i]]$site$geography, quiet = TRUE)
    }
    
    # Altitude
    if(is.null(result[2]$data[[i]]$site$altitude)){
      elev <- result[2]$data[[i]]$sites$site$altitude
      if(is.logical(elev)){
        elev <- NA_integer_}
    }else{
      elev <- result[2]$data[[i]]$site$altitude
      if(is.logical(elev)){
        elev <- NA_integer_}
    }
    
    # Description
    if(is.null(result[2]$data[[i]]$site$sitedescription)){
      description <- result[2]$data[[i]]$sites$site$sitedescription
      if(is.logical(description)){
        description <- NA_character_}
    }else{
      description <- result[2]$data[[i]]$site$sitedescription
      if(is.logical(description)){
        description <- NA_character_}
    }

    # Notes
    if(is.null(result[2]$data[[i]]$site$sitenotes)){
      notes <- result[2]$data[[i]]$sites$site$sitenotes
      if(is.logical(notes)){
        notes <- NA_character_}
    }else{
      notes <- result[2]$data[[i]]$site$sitenotes
      if(is.logical(notes)){
        notes <- NA_character_}
    }
    
    # Datasets
    # i-th element result[2]$data[[i]]$
    datasets_length <- length(result[2]$data[[i]]$site$datasets)

    for(j in 1:datasets_length) {
      datasetid <- result[2]$data[[i]]$site$datasets[[j]]$datasetid
      datasettype <- result[2]$data[[i]]$site$datasets[[j]]$datasettype
      if(is.null(result[2]$data[[i]]$site$datasets[[j]]$datasetnotes)){
        datasetnotes <- NA_character_
        if(is.logical(datasetnotes)){
          datasetnotes <- NA_character_}
      }else{
        #datasetnotes <- result[2]$data[[i]]$site$datasets[[j]]$datasetnotes
        datasetnotes <- result[2]$data[[i]]$site$datasets[[j]]$datasetnotes
        if(is.logical(datasetnotes)){
          datasetnotes <- NA_character_}
      }

      new_dataset <- new('dataset',
                       datasetid = datasetid,
                       datasetname = sitename,
                       datasettype = datasettype,
                       location = location,
                       notes = datasetnotes)
    }
    dataset_list <- append(dataset_list, new_dataset)
    datasets_list <- new('datasets', datasets = dataset_list)
    
    
    ## Collunits
    # Coll Unit ID
    if(is.null(result[2]$data[[i]]$site$collectionunitid)){
      collunitid <- result[2]$data[[i]]$sites$site$collectionunitid
    }else{
      collunitid <- result[2]$data[[i]]$site$collectionunitid
    }

    
    colldate = as.Date("2007-02-01")
    
    # Coll Unit Handle
    if(is.null(result[2]$data[[i]]$site$handle)){
      handle <- result[2]$data[[i]]$sites$site$handle
    }else{
      handle <- result[2]$data[[i]]$site$handle
    }
    
    new_collunit <- new("collunit",
                        collunitid = collunitid,
                        colldate = colldate,
                        handle = handle,
                        datasets = datasets_list)
    
    coll_units <- append(coll_units, new_collunit)
    coll_units <- new('collunits', collunits = coll_units)

    new_site <- new("site",
                    siteid = siteid,
                    sitename = sitename,
                    location = location, 
                    altitude = elev,
                    description = description,
                    notes = NA_character_,
                    collunits = coll_units)
    sites <- append(sites, new_site)
  }  

    sites <- new('sites', sites = sites)

  return(sites)

}

#' @title Get Dataset Numeric
#' @import lubridate
#' @importFrom methods new
#' @param datasetid Use a single number to extract site information
#' @param ... arguments in ellipse form
#' @export
get_datasets.numeric <- function(datasetid, ..., verbose =0) {
  
  # Delete print once it works properly
  #print("Getting in Datasets Numeric")
  useNA <- function(datasetid, type) {
    if (is.na(datasetid)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(datasetid)
    }
  }
  
  if (length(datasetid) > 0) {
    dataset <- paste0(datasetid, collapse = ',')
  }
  
  baseURL <- paste0('data/datasets/', dataset)
  
  result <- parseURL(baseURL)
  #Print when debugging
  #print(result)
  output <- parse_dataset(result)
  
  if(verbose == 1){
    cat("A site object containing", length(result[2]$data), "sites and 6 parameters. \n")
  }

  return(output)
}



#' @title Get Dataset Default
#' @import lubridate
#' @importFrom methods new
#' @param datasetid Use a single number to extract site information
#' @param ... arguments in ellipse form
#' @param datasettype A character string corresponding to one of the allowed dataset types in the Neotoma Database.  Allowed types include: \code{"geochronologic"}, \code{"loss-on-ignition"}, \code{"pollen"}, \code{"plant macrofossils"}, \code{"vertebrate fauna"}, \code{"mollusks"}, and \code{"pollen surface sample"}.  See note in Details delow.
#' @param piid Numeric value for the Principle Investigator's ID number.
#' @param altmin Numeric value indicating the minimum altitude for the site (can be used alone or with \code{altmax}).
#' @param altmax Numeric value indicating the maximum altitude for the site (can be used alone or with \code{altmin}).
#' @param loc A GeoJSON string \code{\{'type':'Polygon', 'coords':c(c(lat,long),c(lat, long),...,c(lat, long))\}} representing the bounding box within which to search for sites.
#' @param taxonids A numeric identifier for the taxon.  See \code{\link{get_table}} and use \code{get_tables('Taxa')} for a list of acceptable values.
#' @param taxonname A character string corresponding to a valid taxon identity in the Neotoma Database.  See \code{\link{get_table}} and use \code{get_table('Taxa')} for a list of acceptable values.
#' @param ageold The oldest date acceptable for the search (in years before present).
#' @param ageyoung The youngest date acceptable for the search.
#' @param ageof If a taxon ID or taxon name is defined this parameter must be set to \code{"taxon"}, otherwise it may refer to \code{"sample"}, in which case the age bounds are for any samples within datasets or \code{"dataset"} if you want only datasets that are within the bounds of ageold and ageyoung.
#' @param subdate Date of dataset submission.
#' @export
get_datasets.default <- function(..., verbose = 0) {
  
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  
  error_check <- check_args(cl)
  
  if (error_check[[2]]$flag == 1) {
    stop(paste0(unlist(error_check[[2]]$message), collapse = '\n  '))
  } else {
    cl <- error_check[[1]]
  }
  
  baseURL <- paste0('data/datasets')
  result <- parseURL(baseURL, ...) %>% 
    cleanNULL()
  
  if(is.null(result$data[1][[1]])){
    output <- cat("I can't find a site for you. Are you using the right spelling? \n\n")
    return(output)
  }else{
    output <- parse_dataset(result)
    args <- list(...)
  }
  
  if(verbose == 1){
    cat("A site object containing", length(result[2]$data), "sites and 5 parameters. \n")
  }
  
  return(output)
}
