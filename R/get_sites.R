#' @title Get Site Information for Fossil Sites
#' @export
get_site <- function(x = NA, sitename, altmin, altmax, loc, gpid, ...) {
  if(!missing(x)) {
    UseMethod('get_site', x)
  } else {
    UseMethod('get_site', NA)
  }
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
    } else {
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

  return(output)

}
