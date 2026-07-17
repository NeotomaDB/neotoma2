#' @title records_to_df
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description Helper that turns a list of API records (each a named list of
#' scalar fields) into a single `data.frame`, building it column by column in
#' one pass. This replaces a per-record `map()` + `bind_rows()`, which is the
#' main cost when a sample carries hundreds of `datum` rows. `NULL` (or empty)
#' fields are filled with `NA`, and the column order follows the order in which
#' the fields first appear.
#' @param records A list of named lists, e.g. `sample$datum` or `sample$ages`.
#' @param tibble If TRUE return a `tbl_df` (as `datum` did), otherwise a plain
#' `data.frame` (as `ages` did). This keeps the output class identical to the
#' previous `bind_rows()` implementation.
#' @importFrom dplyr as_tibble
#' @returns A table with one row per record.
#' @noRd
records_to_df <- function(records, tibble = TRUE) {
  if (length(records) == 0) {
    return(dplyr::as_tibble(list()))
  }
  keys <- unique(unlist(lapply(records, names)))
  cols <- lapply(keys, function(k) {
    vals <- lapply(records, function(r) {
      v <- r[[k]]
      if (is.null(v) || length(v) == 0) NA else v
    })
    unlist(vals, use.names = FALSE)
  })
  names(cols) <- keys
  if (tibble) {
    dplyr::as_tibble(cols)
  } else {
    as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
  }
}

#' @title Build a samples `data.frame` from Neotoma API JSON
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @importFrom methods new
#' @description
#' Helper function to build a sample from the API input (list formatted)
#' coming from the Neotoma API.
#' @param x sample list
#' @returns A simple `sample` object
#' @noRd
build_sample <- function(x) {
  # A sample coming from the API must carry a real sampleid. When it does not,
  # the element is malformed/empty, so we drop it rather than build a phantom
  # sample. The caller null-filters before building the `samples` container.
  if (is.null(x$sampleid)) {
    return(NULL)
  }
  df <- records_to_df(x$datum)
  df_age <- records_to_df(x$ages, tibble = FALSE)
  analyst_list_helper <- x$sampleanalyst %>%
                            map(function(y) {
                              y$contactname
                            })
  new_sample <- new("sample",
                    ages = df_age,
                    igsn = use_na(x$igsn, "char"),
                    datum = df,
                    depth = use_na(x$depth, "int"),
                    sampleid = use_na(x$sampleid, "int"),
                    thickness = use_na(x$thickness, "int"),
                    samplename = use_na(x$samplename, "char"),
                    sampleanalyst = analyst_list_helper,
                    analysisunitid = use_na(x$analysisunitid, "int"),
                    analysisunitname = use_na(x$analysisunitname, "char"))
  return(new_sample)
}
