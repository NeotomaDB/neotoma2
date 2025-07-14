#' @title Build a dataset object from a JSON list representation.
#' @author Socorro Dominguez
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Helper function to build a dataset from the API JSON response.
#' @param args a JSON dataset object passed from the Neotoma API.
#' @returns A simple `dataset` object.
#' @keywords internal
#' @noRd
build_dataset <- function(...) {
    args <- list(...)
    assertthat::assert_that(is.list(args),
                            msg = "Parsed object must be a list.")
  # PI Information
  pi_list <- testNull(args$datasetpi, list())
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
  ds <- set_dataset(
      datasetid = use_na(testNull(args$datasetid, NA), "int"),
      database = use_na(testNull(args$database, NA), "char"),
      doi = list(args$doi),
      datasettype = use_na(testNull(args$datasettype, NA), "char"),
      datasetname = use_na(testNull(args$datasetname, NA), "char"),
      age_range_old = use_na(testNull(args$age_range_old, NA), "int"),
      age_range_young = use_na(testNull(args$age_range_young, NA), "int"),
      age_units = use_na(testNull(args$units, NA), "char"),
      notes = use_na(testNull(args$datasetnotes, NA), "char"),
      pi_list = pi_list,
      samples = testNull(args$samples, NULL),
      specimens = testNull(args$specimens, NULL))
}