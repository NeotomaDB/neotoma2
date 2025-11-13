#' @title set Sample Information
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom uuid UUIDgenerate
#' @importFrom digest digest
#' @importFrom methods is new slot<-
#' @param x Object to be set as a sample
#' @param ages ages
#' @param igsn IGSN character
#' @param datum dataframe of datum
#' @param depth integer representing depth
#' @param sampleid ID for sample
#' @param thickness thickness of core
#' @param samplename sample's name
#' @param sampleanalyst Analyst's contact name
#' @param analysisunitid Which analysis unit it is
#' @param analysisunitname Analysis Unit's name
#' @returns `sample` object
#' @description Function to create new samples objects for analysis. 
#' The new object will not be uploaded to the database.
#' @export
#' @md
#' @examples {
#' # Set an empty sample
#' my_sample <- set_sample()
#' }
set_sample <- function(x = NA,
                       ages = list(),
                       igsn = NA_character_,
                       datum = data.frame(),
                       depth = NA_integer_,
                       sampleid = NA_integer_,
                       thickness = NA_integer_,
                       samplename = NA_character_,
                       sampleanalyst = list(),
                       analysisunitid = NA_integer_,
                       analysisunitname = NA_character_){
  function_call <- match.call()
  if (suppressWarnings(is.na(x))) {
    x <- new("sample")
    if (is.na(sampleid)) {
      hash <- digest(UUIDgenerate(), algo = "xxhash32", serialize = FALSE)
      x@sampleid <- as.integer(strtoi(substr(hash, 1, 7), base = 16L))
    } else {
      x@sampleid <- sampleid
    }
    x@ages <- ages
    x@igsn <- igsn
    x@datum <- datum
    x@depth <- depth
    x@thickness <- thickness
    x@samplename <- samplename
    x@sampleanalyst <- sampleanalyst
    x@analysisunitid <- analysisunitid
    x@analysisunitname <- analysisunitname
  } else {
    if (is(x, "sample")) {
      if (length(function_call) > 2) {
        for (i in 3:length(function_call)) {
          slot(x, names(function_call)[[i]]) <- eval(function_call[[i]])
        }
        return(x)
      } else {
        return(x)
      }
    } else {
      stop("`x` must be a sample object if it is supplied.")
    }
  }
  return(x)
}