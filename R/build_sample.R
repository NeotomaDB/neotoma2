#' @title build_sample
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Helper function to build a sample
#' @param x sample list
#' @return list parsed into samples
#' @export
#' @examples \dontrun{
#' # To build sample from API call:
#' build_sample(x)
#' }
#'
build_sample <- function(x) {

  df <- x$datum %>%
    map(function(y) {
      y <- map(y, testNull)
      as.data.frame(y)
    }) %>%
    bind_rows()

  df_age <- x$ages %>%
   map(function(y) {
     y <- map(y, testNull)
     as.data.frame(y)
   }) %>%
   bind_rows()

  # Analyst Info
  analyst_list_helper <- x$sampleanalyst %>%
    map(function(y) {
      y$contactname
    })

  new_sample <- new("sample",
      ages = df_age,
      igsn = use_na(testNull(x$igsn, NA), "char"),
      datum = df,
      depth = use_na(testNull(x$depth, NA), "int"),
      sampleid = use_na(testNull(x$sampleid, NA), "int"),
      thickness = use_na(testNull(x$thickness, NA), "int"),
      samplename = use_na(testNull(x$samplename, NA), "char"),
      sampleanalyst = analyst_list_helper,
      analysisunitid = use_na(testNull(x$analysisunitid, NA), "int"),
      analysisunitname = use_na(testNull(x$analysisunitname, NA), "char"))

  return(new_sample)

}
