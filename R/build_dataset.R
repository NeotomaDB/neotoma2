#' @title Build a dataset object from a JSON list representation.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @description
#' Helper function to build a dataset from the API JSON response.
#' @param args a JSON dataset object passed from the Neotoma API.
#' @returns A simple `dataset` object.
#' @noRd
build_dataset <- function(...) {
  args <- list(...)
  assert_that(is.list(args), msg = "Parsed object must be a list.")
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
  args <- args %>% cleanNULL()
  ds <- set_dataset(datasetid = use_na(args$datasetid, "int"),
                    database = use_na(args$database, "char"),
                    doi = list(args$doi),
                    recdatecreated = use_na(args$recdatecreated, "date"),
                    datasettype = use_na(args$datasettype, "char"),
                    datasetname = use_na(args$datasetname, "char"),
                    age_range_old = use_na(args$age_range_old, "int"),
                    age_range_young = use_na(args$age_range_young, "int"),
                    age_units = use_na(args$units, "char"),
                    notes = use_na(args$datasetnotes, "char"),
                    pi_list = pi_list,
                    samples = testNull(args$samples, NULL),
                    specimens = testNull(args$specimens, NULL))
  return(ds)
}