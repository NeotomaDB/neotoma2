utils::globalVariables(c("analysisunitid"))

#' @title Extract datasets from a sites object.
#' @description If the sites object contains datasets, then the datasets
#' will be returned. If the sites object does not contain datasets then
#' the user can apply `get_datasets()` to the object.
#' @param object A sites object
#' @returns datasets object specific to the metadata contained in datasets
#' @export
setGeneric("datasets",
           function(object) {
             standardGeneric(f = "datasets")
           })

#' @title Obtain coordinates from a sites object.
#' @param obj A sites object
#' @param ... Additional parameters associated with the call.
#' @returns dataframe with coordinate values
#' @export
setGeneric("coordinates", function(obj, ...) {
  standardGeneric(f = "coordinates")
})

#' @title plotLeaflet
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description Plot sites on a leaflet map
#' @param object Sites object to plot
#' @returns leaflet map with site markers
#' @export
setGeneric("plotLeaflet", function(object) {
  standardGeneric(f = "plotLeaflet")
})

#' @title Show matches for objects.
#' @param x object to show matches for
#' @returns data.frame that marks if a `site` exists in another `sites` object
#' @export
setGeneric("showMatch", function(x) {
  standardGeneric(f = "showMatch")
})

#' @title Obtain samples from a record or multiple records.
#' @param x sites object
#' @returns data.frame with record information at sample level
#' @export
setGeneric("samples", function(x) {
  standardGeneric(f = "samples")
})

# #' @title Obtain specimens from a record or multiple records.
# #' @param x sites object
# #' @returns data.frame with record information regarding specimens
# #' @export
# setGeneric("specimens", function(x) {
#   standardGeneric(f = "specimens")
# })

#' @title Obtain the chronology from a record or multiple records.
#' @param x sites object that contains chronologies
#' @returns chronologies object with all chronologies used.
#' @export
setGeneric("chronologies", function(x) {
  standardGeneric(f = "chronologies")
})

#' @title Obtain the DOI for publications or datasets.
#' @param x Object with DOIs associated to it.
#' @returns doi object with DOI information
#' @export
setGeneric("doi", function(x) {
  standardGeneric(f = "doi")
})

#' @title Generate a data citation from a Neotoma2 object.
#' @description The function, applied to a data object with a valid dataset, 
#' will return a properly formatted data citation for the record.
#' @param x Object with DOIs associated to it.
#' @returns data.frame with citation data
#' @export
setGeneric("cite_data", function(x) {
  standardGeneric(f = "cite_data")
})

#' @title Select the best match for an object.
#' @param x object
#' @param n n elements that are a best match
#' @returns attr Select the match between a local record and a Neotoma match
#' @export
setGeneric("selectMatch", function(x, n) {
  standardGeneric(f = "selectMatch")
})

#' @title chroncontrols
#' @description Show the samples table
#' @param x Sites object to extract chroncontrols table from
#' @returns data.frame with chroncontrols information
#' @export
setGeneric("chroncontrols", function(x) {
  standardGeneric(f = "chroncontrols")
})


#' @title taxa
#' @description Show the samples table
#' @param object Sites object to extract taxa table from
#' @returns data.frame with taxa records
#' @export
setGeneric("taxa", function(object) {
  standardGeneric(f = "taxa")
})

# @title toJSON
# @author Socorro Dominguez 
# @description Export toJSON
# @param x Sites object to extract taxa table from
# @returns JSON translation of `sites` object to JSON
# @keywords internal
# @noRd
# # comment out for now
# setGeneric("toJSON", function(x) {
#   standardGeneric(f = "toJSON")
# })

#' @title Add a new chronology to a collection unit.
#' @param object A collectionunit object
#' @param x A chronology object
#' @param y A \code{data.frame} of sample ages
#' @returns chronology object defined by user,
#' @export
setGeneric("add_chronology",
           function(object, x, y) {
             standardGeneric(f = "add_chronology")
           })

#' @title Extract collection units from a sites object
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @returns collunits detail from a sites object
#' @export
setGeneric("collunits",
           function(object) {
             standardGeneric(f = "collunits")
           })

#' @title Set the default chronology within a collectionunit.
#' @param x A chronologies object.
#' @param n The particular chronology to be used as the default.
#' @returns sites object with new default chronology
#' @export
setGeneric("set_default",
           function(x, n) {
             standardGeneric(f = "set_default")
           })