#' @title to_json
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import sf
#' @import jsonify
#' @import geojsonsf
#' @description
#' Convert a Site object into a json file for API management
#' @param x sites R object to be converted
#' @return The function returns a json object
#' @examples \dontrun{
#' # To find all sites that contain the string "Alex%"
#' alex.sites <- get_sites(sitename="Alex%")
#'
#' Convert the object to json
#' to_json(alex.sites)
#' @export
to_json.sites <- function(x = NA, ...) {
  
  output <- jsonify::to_json(
    purrr::map(x@sites, function(y){
      list(
           area = "missing",
           notes = "missing",
           siteid = y@siteid,
           altitude = y@altitude,
           sitename = y@sitename,
           geography = geojsonsf::sf_geojson(y@geography),
           geopolitical = "missing",
           collectionunits = purrr::map(y@collunits@collunits, function(z){
                                  list(
                                       notes = "missing",
                                       handle = z@handle,
                                       colldate = "missing",
                                       location = "missing",
                                       waterdepth = "missing",
                                       gpslocation = "missing",
                                       collunittype = "missing",
                                       collectionunit = z@collectionunitname,
                                       collectiondevice = "missing",
                                       collectionunitid = z@collectionunitid,
                                       depositionalenvironment = "missing",
                                       datasets = purrr::map(z@datasets@datasets, function(d){
                                                      list(
                                                           doi = "missing",
                                                           agerange = "missing",
                                                           database = d@database,
                                                           datasetid = d@datasetid,
                                                           datasetpi = "missing",
                                                           datasetname = d@datasetname,
                                                           datasettype = d@datasettype,
                                                           datasetnotes = d@notes,
                                                           samples = purrr::map(d@samples@samples, function(s){
                                                             list(
                                                               ages = "missing",
                                                               igsn = "missing",
                                                               datum = "missing",
                                                               depth = "missing",
                                                               sampleid = "missing",
                                                               thickness = "missing",
                                                               samplename = "missing",
                                                               sampleanalyst = "missing",
                                                               analysisunitid = "missing",
                                                               analysisunitname = "missing"
                                                             )})
                                                           )}),
                                       chronologies = purrr::map(z@chronologies@chronologies, function(d){
                                                          list(
                                                               chronology = "missing",
                                                               contact = "missing",
                                                               agemodel = "missing",
                                                               agerange = "missing",
                                                               isdefault = "missing",
                                                               dateprepared = "missing",
                                                               modelagetype = "missing",
                                                               chronologyname = "missing",
                                                               chronologyid = "missing",
                                                               chroncontrols = "missing",
                                                               depth = "missing",
                                                               geochron = "missing",
                                                               thickness = "missing",
                                                               agelimitoder = "missing",
                                                               chroncontrolid = "missing",
                                                               agelimityounger = "missing",
                                                               chroncontrolage = "missing",
                                                               chroncontroltype = "missing",
                                                               defaultchronology = "missing"
                                                               )}),
                                       defaultchronology = "missing"
                                       )}),
           sitedescription = y@description
           )}), unbox = TRUE)
  
  return(output)
}

