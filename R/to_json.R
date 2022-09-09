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
           area = y@area,
           notes = y@notes,
           siteid = y@siteid,
           altitude = y@altitude,
           sitename = y@sitename,
           geography = geojsonsf::sf_geojson(y@geography),
           geopolitical = y@geopolitical,
           collectionunits = purrr::map(y@collunits@collunits, function(z){
                                  list(
                                       notes = z@notes,
                                       handle = z@handle,
                                       colldate = z@colldate,
                                       location = z@location,
                                       waterdepth = z@waterdepth,
                                       gpslocation = geojsonsf::sf_geojson(z@gpslocation),
                                       collunittype = z@collunittype,
                                       collectionunit = z@collectionunitname,
                                       collectiondevice = z@collectiondevice,
                                       collectionunitid = z@collectionunitid,
                                       depositionalenvironment = z@depositionalenvironment,
                                       datasets = purrr::map(z@datasets@datasets, function(d){
                                                      list(
                                                           doi = d@doi,
                                                           agerange = list(agerangeold = d@age_range_old,
                                                                           agerangeyoung = d@age_range_young),
                                                           database = d@database,
                                                           datasetid = d@datasetid,
                                                           datasetpi = d@pi_list,
                                                           datasetname = d@datasetname,
                                                           datasettype = d@datasettype,
                                                           datasetnotes = d@notes,
                                                           samples = purrr::map(d@samples@samples, function(s){
                                                             list(
                                                               ages = s@ages,
                                                               igsn = s@igsn,
                                                               datum = s@datum,
                                                               depth = s@depth,
                                                               sampleid = s@sampleid,
                                                               thickness = s@thickness,
                                                               samplename = s@samplename,
                                                               sampleanalyst = s@sampleanalyst,
                                                               analysisunitid = s@analysisunitid,
                                                               analysisunitname = s@analysisunitname
                                                             )})
                                                           )}),
                                       chronologies = purrr::map(z@chronologies@chronologies, function(ch){
                                                          list(
                                                               #chronology = "missing",
                                                               contact = ch@contact,
                                                               agemodel = ch@agemodel,
                                                               agerange = list(ageboundolder = ch@ageboundolder, 
                                                                               ageboundyounger = ch@ageboundyounger),
                                                               isdefault = ch@isdefault,
                                                               dateprepared = ch@dateprepared,
                                                               modelagetype = ch@modelagetype,
                                                               chronologyname = ch@chronologyname,
                                                               chronologyid = ch@chronologyid,
                                                               chroncontrols = ch@chroncontrols
                                                               )}),
                                       defaultchronology = z@defaultchronology
                                       )}),
           sitedescription = y@description
           )}), unbox = TRUE)
  
  return(output)
}

