#' @title Get closest
#' @importFrom utils write.csv write.table
#' @importFrom assertthat assert_that
#' @import sf
#' @param x A vector long/lat pair, or a dataset, site or download.
#' @param n The maximum number of records to return (in the case of ties the return may be larger)
#' @param buffer The size of the buffer for dataset search (in kilometers)
#' @param ... optional arguments to pass into \code{get_dataset}.
#' @export
get_closest <- function(x, n, buffer, ...) {
  UseMethod('get_closest')
}

#' @export
get_closest.default <- function(x, n, buffer, ...) {
  
  assertthat::assert_that(length(x) == 2, 
                          msg = "You must pass an x, y object (e.g., c(10, -20)).")
  assertthat::assert_that(x[1] >= -180 & x[1] <= 180, 
                          msg = "You must pass an array of long/lat with long values from -180 - 180.")
  assertthat::assert_that(x[2] >= -90 & x[2] <= 90, 
                          msg = "You must pass an array of long/lat with lat values from -90 - 90.")
  
  utm_w <- seq(-180, 180, by = 6)
  
  proj <- paste0("+proj=utm +zone=",findInterval(x[1], utm_w))
  
  if (x[2] < 0) proj <- paste0(proj, " +south")
  
  bbox <- sf::st_point(x) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(4326) %>% 
    sf::st_transform(crs = proj) %>%
    sf::st_buffer(buffer * 1000) %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>% 
    as.numeric()
  
  #buff_sets <- suppressMessages(get_datasets(loc = bbox, ...))
  
  return(bbox)
}


#' @export
get_closest.site <- function(x, n, buffer, ...) {
  coords <- c(x$long, x$lat)
  get_closest(coords, n, buffer, ...)
}


