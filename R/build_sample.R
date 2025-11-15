#' @title Build a samples `data.frame` from Neotoma API JSON
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom methods new
#' @description
#' Helper function to build a sample from the API input (list formatted)
#' coming from the Neotoma API.
#' @param x sample list
#' @returns A simple `sample` object
#' @noRd
build_sample <- function(x) {
  df <- x$datum %>%
          map(function(y) {
            y <- map(y, testNull)
          }) %>%
          bind_rows()
  df_age <- x$ages %>%
              map(function(y) {
                y <- map(y, testNull)
                as.data.frame(y)
              }) %>%
              bind_rows()
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
