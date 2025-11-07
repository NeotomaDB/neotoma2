#' @title plotLeaflet
#' @description Plot sites on a leaflet map
#' @importFrom purrr map
#' @importFrom leaflet leaflet addTiles addCircleMarkers 
#' @importFrom leaflet markerOptions markerClusterOptions
#' @param object Sites object to plot
#' @examples \donttest{
#' # Note that by default the limit for queries is 25 records:
#' modernSites <- get_sites(keyword = "Modern")
#' plotLeaflet(modernSites)
#' }
#' @returns `leaflet` map
#' @export
setMethod(f = "plotLeaflet",
          signature = "sites",
          definition = function(object) {
            explorerURL <- "http://apps.neotomadb.org/explorer/"
            df1 <- as.data.frame(object)
            map1 <- leaflet(df1) %>%
              addTiles() %>%
              addCircleMarkers(lng = df1$long,
                               lat = df1$lat,
                               popup = paste0("<b>", df1$sitename,
                                              "</b><br><b>Description:</b> ",
                                              df1$description,
                                              "<br><a href=", 
                                              explorerURL, "?siteids=",
                                              df1$siteid,
                                              ">Explorer Link</a>"),
                               clusterOptions = markerClusterOptions(),
                               options = markerOptions(riseOnHover = TRUE))
            return(map1)
          })