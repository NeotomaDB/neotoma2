#' @md
#' @title get_stats
#' @author Socorro Dominguez \email{s.dominguez@ht-data.com}
#' @author Simon Goring \email{goring@wisc.edu}
#' @import dplyr
#' @import tidyr
#' @description Returns the count of sites, datasets, publications and other 
#' objects added to Neotoma during the requested time period.
#' @param start The starting month (from present == 0) for which to generate the summary.
#' @param end The ending month (from present == 0) for which to generate the summary.
#' @returns `data.frame` with summary statistics
#' @export
get_stats <- function(start, end) {
  
  base_url <- paste0("/data/summary/rawbymonth?start=",start,"&end=", end)
  result <- neotoma2::parseURL(base_url)
  
  result <- result$data$data

  df <- result %>% purrr::map_df(~ data.frame(
    datasets = .$datasets,
    sites = .$sites,
    publications = .$publications,
    authors = .$authors,
    countrygpid = .$countrygpid
  ))
  
  return(df)
  
}