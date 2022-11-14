utils::globalVariables(c("analysisunitid"))

#' @title Extract datasets from a sites object.
#' @param object A sites object
#' @export
# Todo Convert to as.data.frame
setGeneric("datasets",
           function(object) {
             standardGeneric(f = "datasets")
           })

#' @title Obtain coordinates from a sites object.
#' @param obj A sites object
#' @param ... Additional parameters associated with the call.
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @export
# Todo Convert to as.data.frame
setGeneric("coordinates", function(obj, ...) {
  standardGeneric(f = "coordinates")
})

#' @title plotLeaflet
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import sf
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @import arulesViz
#' @import leaflet
#' @import mapview
#' @description Plot sites on a leaflet map
#' @param object Sites object to plot
#' @param save_im save output
#' @param path location where output should be saved in. save_im must be TRUE
#' @export
setGeneric("plotLeaflet", function(object, save_im = FALSE, path = "") {
  standardGeneric(f = "plotLeaflet")
})

#' @title Show matches for objects.
#' @param x object to show matches for
#' @export
setGeneric("showMatch", function(x) {
  standardGeneric(f = "showMatch")
})

#' @title Obtain samples from a record or multiple records.
#' @param x sites object
#' @export
setGeneric("samples", function(x) {
  standardGeneric(f = "samples")
})

#' @title Obtain the chronology from a record or multiple records.
#' @export
#' @param x sites object that contains chronologies
setGeneric("chronologies", function(x) {
  standardGeneric(f = "chronologies")
})

#' @title Obtain the DOI for publications or datasets.
#' @param x Object with DOIs associated to it.
#' @export
setGeneric("doi", function(x) {
  standardGeneric(f = "doi")
})

#' @title Obtain the DOI for publications or datasets.
#' @param x Object with DOIs associated to it.
#' @export
setGeneric("cite_data", function(x) {
  standardGeneric(f = "cite_data")
})

#' @title Select the best match for an object.
#' @param x object
#' @param n n elements that are a best match
#' @export
setGeneric("selectMatch", function(x, n) {
  standardGeneric(f = "selectMatch")
})

#' @title chroncontrols
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @description Show the samples table
#' @param x Sites object to extract chroncontrols table from
#' @export
setGeneric("chroncontrols", function(x) {
  standardGeneric(f = "chroncontrols")
})


#' @title taxa
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @description Show the samples table
#' @param object Sites object to extract taxa table from
#' @export
setGeneric("taxa", function(object) {
  standardGeneric(f = "taxa")
})

#' @title toJSON
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @description Export toJSON
#' @param x Sites object to extract taxa table from
#' @export
setGeneric("toJSON", function(x) {
  standardGeneric(f = "toJSON")
})

#' @title Add a new chronology to a collection unit.
#' @param object A collectionunit object
#' @param x A chronology object
#' @param y A \code{data.frame} of sample ages
#' @importFrom methods slotNames slot
#' @export
setGeneric("add_chronology",
           function(object, x, y) {
             standardGeneric(f = "add_chronology")
           })

#' @title Extract collection units from a sites object
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @export
setGeneric("collunits",
           function(object) {
             standardGeneric(f = "collunits")
           })

#' @title Set the default chronology within a collectionunit.
#' @param x A chronologies object.
#' @param n The particular chronology to be used as the default.
#' @importFrom methods slotNames slot
#' @export
setGeneric("set_default",
           function(x, n) {
             standardGeneric(f = "set_default")
           })
