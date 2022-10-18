#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Helper function to build a dataset
#' @param x dataset list
#' @return list parsed into datasets
#' @export
#' @examples \dontrun{
#' # To build dataset from API call:
#' build_dataset(x)
#' }
#'
build_dataset <- function(x) {

  samples <- purrr::map(x$samples, build_sample)
  samples <- new("samples", samples = samples)

  # PI Information
  pi_list <- testNull(x$datasetpi, list())
  if (length(pi_list) != 0) {
    pi_list <- pi_list %>%
      map(function(y) {
        if (is.na(y[1])) {
          NA_character_
        } else {
          y$contactname
        }
      })
  }
  
  specimens <- new("specimens", specimens = list())

  new("dataset",
      datasetid = use_na(testNull(x$datasetid, NA), "int"),
      database = use_na(testNull(x$database, NA), "char"),
      doi = list(x$doi),
      datasettype = use_na(testNull(x$datasettype, NA), "char"),
      datasetname = use_na(testNull(x$datasetname, NA), "char"),
      age_range_old = use_na(testNull(x$agerange[[1]]$ageold, NA), "int"),
      age_range_young = use_na(testNull(x$agerange[[1]]$ageyoung, NA), "int"),
      notes = use_na(testNull(x$datasetnotes, NA), "char"),
      pi_list = pi_list,
      samples = samples,
      specimens = specimens)

}
