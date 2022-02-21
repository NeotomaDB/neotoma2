#' @title Extract datasets from a sites object.
#' @param object A sites object
#' @export
# Todo Convert to as.data.frame
setGeneric("datasets",
           function(object) {
             standardGeneric("datasets")
           })

#' @title Obtain coordinates from a sites object.
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @export
# Todo Convert to as.data.frame
setGeneric("coordinates", function(obj, ...) {
  standardGeneric("coordinates")
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
  standardGeneric("plotLeaflet")
})

#' @title Show matches for objects.
#' @export
setGeneric("showMatch", function(x) {
  standardGeneric("showMatch")
})

#' @title Obtain samples from a record or multiple records.
#' @export
setGeneric("samples", function(x) {
  standardGeneric("samples")
})

#' @title Obtain the chronology from a record or multiple records.
#' @export
setGeneric("chronologies", function(x) {
  standardGeneric("chronologies")
})

#' @title Obtain the DOI for publications.
#' @export
setGeneric("doi", function(x) {
  standardGeneric("doi")
})

#' @title Select the best match for an object.
#' @export
setGeneric("selectMatch", function(x, n) {
  standardGeneric("selectMatch")
})

#' @title taxa
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @description Show the samples table
#' @param object Sites object to extract taxa table from
#' @export
setGeneric("taxa", function(object) {
  standardGeneric("taxa")
})

#' @title Extract collection units from a sites object
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @export
setGeneric("collunits",
           function(object) {
             standardGeneric("collunits")
           })
