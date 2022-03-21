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
setGeneric("plotLeaflet", function(object, save_im=FALSE, path = "") {
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

#' @title Obtain the DOI for publications.
#' @param x Object with DOIs associated to it.
#' @export
setGeneric("doi", function(x) {
  standardGeneric(f = "doi")
})

#' @title Select the best match for an object.
#' @param x object
#' @param n n elements that are a best match
#' @export
setGeneric("selectMatch", function(x, n) {
  standardGeneric(f = "selectMatch")
})

#' @title taxa
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @description Show the samples table
#' @param object Sites object to extract taxa table from
#' @export
setGeneric("taxa", function(object) {
  standardGeneric(f = "taxa")
})

#' @title Extract collection units from a sites object
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @export
setGeneric("collunits",
           function(object) {
             standardGeneric(f = "collunits")
           })
