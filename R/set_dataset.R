#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @importFrom methods slot<-
#' @param x object to be set as dataset,
#' @param datasetid dataset identifier
#' @param database dataset where the dataset came from
#' @param doi doi
#' @param datasettype type the dataset belongs to
#' @param age_range_old age range old
#' @param age_range_young age range young
#' @param notes notes
#' @param pi_list pi list
#' @param samples
#' @export
#' @examples
#' \dontrun{
#' # Create a dataset 
#' my_dataset <- set_dataset(database = "EPD",
#'                     datsettype = "pollen",
#'                     notes = "my lake"0)
#' }

set_dataset <- function(x = NA,
                        datasetid = NA_integer_,
                        database = NA_character_,
                        doi = NA,
                        datasettype = NA_character_,
                        age_range_old = NA_integer_,
                        age_range_young = NA_integer_,
                        notes = NA_character_,
                        pi_list = NA,
                        samples = new("samples")) {
  
  function_call <- match.call()
  
  if (suppressWarnings(is.na(x))) {
    x <- new("dataset")
    if (is.na(datasetid)) {
      x@datasetid <- uuid::UUIDgenerate()
    } else {
      x@datasetid <- datasetid
    }
    x@database <- database
    x@doi <- doi
    x@datasettype <- datasettype
    x@age_range_old <- age_range_old
    x@age_range_young <- age_range_young
    x@notes <- notes
    x@pi_list <- pi_list
    x@samples <- samples
    
  } else {
    if (class(x) == "dataset") {
      for (i in 3:length(function_call)) {
        slot(x, names(function_call)[[i]]) <- function_call[[i]]
      }
    }
    return(x)
  }
  
  return(x)
}
