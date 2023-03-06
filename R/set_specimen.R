#' @title set Sample Information
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @importFrom methods slot<-
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
#' @export
#' @examples
#' \dontrun{
#' # Create a site called "My Lake", to
#' x = st_as_sf(st_sfc(st_point(c(5,5))))
#' my_site <- set_site(sitename = "My Lake",
#'                     geography = x,
#'                     description = "my lake",
#'                     altitude = 30)
#' }
set_specimen <- function(x=NA,
                         datasetid = NA_integer_,
                         sampleid = NA_integer_,
                         specimenid = NA_integer_,
                         repository = new("repository"),
                         taxonid = NA_integer_,
                         taxonname = NA_character_,
                         elementtype = NA_character_,
                         symmetry = NA_character_,
                         portion = NA_character_,
                         sex = NA_character_,
                         domesticstatus = NA_character_,
                         taphonomictype = NA_character_,
                         nisp = NA_integer_,
                         preservative = NA_character_,
                         maturity = NA_character_,
                         samplenotes = NA_character_){
  
  
  function_call <- match.call()
  
  if (suppressWarnings(is.na(x))) {
    x <- new("specimen")
    if (is.na(specimenid)) {
      x@specimenid <- uuid::UUIDgenerate()
    } else {
      x@specimenid <- specimenid
    }
    
    x@datasetid <- datasetid
    x@sampleid <- sampleid
    x@repository <- repository
    x@taxonid <- taxonid
    x@taxonname <- taxonname
    x@elementtype <- elementtype
    x@symmetry <- symmetry
    x@portion <- portion
    x@sex <- sex
    x@domesticstatus <- domesticstatus
    x@taphonomictype <- taphonomictype
    x@nisp <- nisp
    x@preservative <- preservative
    x@maturity <- maturity
    x@samplenotes <- samplenotes
    
  } else {
    if (is(x, "specimen")) {
      if(length(function_call)>2){
        for (i in 3:length(function_call)) {
          slot(x, names(function_call)[[i]]) <- eval(function_call[[i]])
        }
        return(x)
      } else {
        return(x)
      }
    } else {
      stop("`x` must be a specimen object if it is supplied.")
    }
  }
  return(x)
}