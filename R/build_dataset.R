#' @title Build a dataset object from a JSON list representation.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @description
#' Helper function to build a dataset from the API JSON response. This is the
#' one place where the API's dataset field names are mapped onto the slots of a
#' `dataset`; `parse_site()` passes the response through untouched.
#' @param x A JSON dataset object passed from the Neotoma API. `samples` and
#' `specimens`, if present, are already built objects.
#' @returns A simple `dataset` object.
#' @noRd
build_dataset <- function(x) {
  assert_that(is.list(x), msg = "Parsed object must be a list.")
  # A dataset coming from the API must carry a real datasetid. When it does
  # not, the element is malformed/empty, so we drop it rather than fabricate a
  # phantom dataset with a random id (see set_dataset()). The caller
  # null-filters before building the `datasets` container.
  if (is.null(x$datasetid)) {
    return(NULL)
  }
  agerange <- first_agerange(x$agerange)
  pi_list <- map(testNull(x$datasetpi, list()), function(y) {
    use_na(y$contactname, "char")
  })
  ds <- set_dataset(datasetid = use_na(x$datasetid, "int"),
                    database = use_na(x$database, "char"),
                    doi = list(x$doi),
                    recdatecreated = use_na(as.Date(x$recdatecreated), "date"),
                    datasettype = use_na(x$datasettype, "char"),
                    datasetname = use_na(x$datasetname, "char"),
                    age_range_old = use_na(agerange$ageold, "int"),
                    age_range_young = use_na(agerange$ageyoung, "int"),
                    age_units = use_na(agerange$units, "char"),
                    notes = use_na(x$datasetnotes, "char"),
                    pi_list = pi_list,
                    samples = testNull(x$samples, NULL),
                    specimens = testNull(x$specimens, NULL))
  return(ds)
}

#' @title first_agerange
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal helper function used to parse age range information.
#' The API reports `agerange` as an array; a dataset carries one age range, so
#' the first element is the one we want.
#' @param agerange age range information from API response
#' @returns list with normalized age range information
#' @noRd
first_agerange <- function(agerange) {
  if (is.null(agerange) || length(agerange) == 0) {
    list(ageold = NA, ageyoung = NA, units = NA)
  } else {
    list(ageold = agerange[[1]]$ageold,
         ageyoung = agerange[[1]]$ageyoung,
         units = agerange[[1]]$units)
  }
}
