#' @title Get Site Information for Fossil Sites
#' @export
get_site <- function(sitename, altmin, altmax, loc, gpid, ...) {

  UseMethod('get_site')

}

#' @title Get Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @export
get_site.numeric <- function(sitename, ...) {

  if (length(sitename) > 0) {
    sitename <- paste0(sitename, collapse = ',')
  }

  baseURL <- paste0('data/sites/', sitename)

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
                        description = result$data[[1]]$sitedescription,
                        notes = NA_character_,
                        collunits = new("collunits",
                                        collunits = collunit))

  return(output)

}
