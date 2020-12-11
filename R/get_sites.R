#' @title Get Site Information for Fossil Sites
#' @export
get_site <- function(x, sitename, altmin, altmax, loc, gpid, ...) {

  UseMethod('get_site')

}

#' @title Get Site Information for Fossil Sites
#' @export
get_site.numeric <- function(sitename, ...) {
  baseURL <- paste0('data/sites/', sitename)

  result <- parseURL(baseURL)

  output <- new("site", siteid = result$data[[1]]$siteid,
                                sitename = result$data[[1]]$sitename,
                                location = "sfg",
                                description = "character",
                                notes = "character",
                                collunits = "collunits")
}
