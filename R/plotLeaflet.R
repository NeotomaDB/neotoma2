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


#' @title plotLeaflet
#' @description Plot sites on a leaflet map
#' @param object Sites object to plot
#' @param save_im save output
#' @param path location where output should be saved in. save_im must be TRUE
#' @export
setMethod(f = "plotLeaflet",
          signature = "sites",
          definition = function(object, save_im=FALSE, path = "") {
            df1 <- map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@location)[, 2]),
                               long = mean(st_coordinates(x@location)[, 1]),
                               elev = x@altitude,
                               description = x@description)
            }) %>%
              bind_rows()
            map1 <- leaflet(df1) %>%
              addProviderTiles(providers$Stamen.TerrainBackground) %>%
              addTiles() %>%
              addCircleMarkers(lng = df1$long, lat = df1$lat,
              popup = paste0("<b>", df1$sitename,
              "</b><br><b>Description:</b> ",
              df1$description,
              "<br><a href=http://apps.neotomadb.org/explorer/?siteids=",
              df1$siteid,
              ">Explorer Link</a>"),
              clusterOptions = markerClusterOptions(),
              options = markerOptions(riseOnHover = TRUE))

            if (save_im == TRUE) {
              mapshot(map1, file = path)
            }
            map1
          })
# End plot methods