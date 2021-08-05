#' @title S4 class for dataset information
#' @description The standard object class for datasets from the Neotoma Paleoecology Database.
#' @import sf
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @import arulesViz
#' @import leaflet
#' @import mapview
dataset <- setClass(
                    # Set the name for the class
                    "dataset",
                    
                    # Define the slots
                    slots = c(datasetid = "numeric",
                              datasetname = "character",
                              datasettype = "character",
                              location = "sf",
                              notes = "character"),
                    
                    # Set the default values for the slot
                    prototype = list(datasetid = NA_integer_,
                                     datasetname = NA_character_,
                                     datasettype = NA_character_,
                                     location = st_sf(st_sfc()),
                                     notes = NA_character_),
)

#' @title S4 class for datasets information
#' @description The grouped class for datasets from the Neotoma Paleoecology Database.
datasets <- setClass(  
                    # Set the name for the class
                    "datasets",
                    
                    # Define the slots
                    slots = c(datasets = "list"),
                    
                    # Validity functions
                    validity = function(object) {
                      all(object@datasets %>%
                            lapply(class) %>%
                            unlist(recursive = FALSE) ==  'dataset')
  })

#' @title S4 class for collunits information
collunit <- setClass(
  # Set the name for the class
  "collunit",
  slots = c(collunitid = "numeric",
            handle = "character",
            collunitname = "character",
            colldate = "Date",
            substrate = "character",
            location = "character",
            datasets = "datasets"),
  prototype = list(collunitid = NA_integer_,
                   handle = NA_character_,
                   collunitname = NA_character_,
                   colldate = "Date",
                   substrate = NA_character_,
                   location = NA_character_,
                   datasets = NULL),
  validity = function(object) {
    !is.na(object@collunitid)
  })

#' An S4 class for Neotoma Collection Units
#' @description Holds Collection unit information from the Neotoma Paleoecology Database.
#' @importFrom purrr map
collunits <- setClass("collunits",
                      representation(collunits = "list"),
                      validity = function(object) {
                        all(map(object@collunits, function(x) { class(x) == "collunit"}) %>%
                              unlist())
                      })

#' An S4 class for site information from the Neotoma Paleoecology Database.
site <- setClass(
  # Set the name for the class
  "site",
  
  # Define the slots
  slots = c(siteid = "numeric",
            sitename = "character",
            location = "sf",
            altitude = "numeric",
            description = "character",
            notes = "character",
            collunits = "collunits"),
  
  # Set the default values for the slot
  prototype = list(siteid = NA_integer_,
                   sitename = NA_character_,
                   location = st_sf(st_sfc()),
                   altitude = NA_integer_,
                   description = NA_character_,
                   notes = NA_character_,
                   collunits = NULL) # check what would really be a NA here
  
  # Add a validity function that can test data consistency.
  # This is not called if you have an initialize function defined!
)

#' An S4 class for multi-site information from the Neotoma Paleoecology Database.
# TODO Add area
sites <- setClass("sites",
                  representation(sites = "list"),
                  validity = function(object) {
                    all(map(object@sites, function(x) { class(x) == "site"}) %>%
                          unlist())
                  })


# Start "Show Method" for all Neotoma Objects
#' @title Show Dataset Method
setMethod(f = "show",
          signature = "dataset",
          definition = function(object){
            print(data.frame(dataset.id = object@datasetid,
                             site.name = object@datasetname,
                             lat = mean(st_coordinates(object@location)[,1]),
                             long = mean(st_coordinates(object@location)[,2]),
                             type = object@datasettype), row.names=FALSE)
          })

#' @title Show Datasets object as a dataframe
setMethod(f = "show",
          signature= "datasets",
          definition = function(object){
            map(object@datasets, function(x) {
              df <- data.frame(dataset.id = x@datasetid,
                               site.name = x@datasetname,
                               lat = mean(st_coordinates(x@location)[,1]),
                               long = mean(st_coordinates(x@location)[,2]),
                               type = x@datasettype)
            }) %>%
              bind_rows() %>%
              print(row.names=FALSE)
          })

#' @title Show Site objects as a dataframe
setMethod(f = "show",
          signature = "site",
          definition = function(object){
            print(data.frame(siteid = object@siteid,
                             sitename = object@sitename,
                             lat = mean(st_coordinates(object@location)[,1]),
                             long = mean(st_coordinates(object@location)[,2]),
                             elev = object@altitude), row.names=FALSE)
          })

#' @title Show Sites objects as a dataframe
setMethod(f = "show",
          signature= "sites",
          definition = function(object){
            map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@location)[,2]),
                               long = mean(st_coordinates(x@location)[,1]),
                               elev = x@altitude)
            }) %>%
              bind_rows() %>%
              print(row.names=FALSE)
          })

# End of Show Methods

# Start "SqBrackets" Methods
#' @title Method
setMethod(f = "[[",
          signature= signature(x = "datasets", i = "numeric"),
          definition = function(x, i){
            object@sites[[i]]
          })

#' Method 
setMethod(f = "[[",
          signature= signature(x = "dataset", i = "numeric"),
          definition = function(x, i){
            object@sites[[i]]
          })
 
# Method 
setMethod(f = "[[",
          signature= signature(x = "sites", i = "numeric"),
          definition = function(x, i){
            object@sites[[i]]
          })

# End "SqBrackets" Methods

# Start "length" Methods
#' @title Length Method
setMethod(f = "length",
          signature= signature(x = "sites"),
          definition = function(x){
            length(x@sites)
          })

setMethod(f = "length",
          signature= signature(x = "datasets"),
          definition = function(x){
            length(x@datasets)
          })

# TODO : After show method for collunits
# setMethod(f = "length",
#           signature= signature(x = "collunits"),
#           definition = function(x){
#             length(x@collunits)
#           })

# End "length" methods

# Start "c" methods
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @title c Method for NULL values
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x ="missingORNULL" , y){
            y
          })

setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(y, x ="missingORNULL" ){
            y
          })


#' @title c Method for sites
setMethod(f = "c",
          signature = signature("sites"),
          definition = function(x, y){
            new('sites',
                sites= unlist(c(x@sites, 
                                       y@sites), recursive = FALSE))
            })

setMethod(f = "c",
          signature = signature("datasets"),
          definition = function(x, y){
            new('datasets',
                datasets= unlist(c(x@datasets, 
                                   y@datasets), recursive = FALSE))
          })

# End "c" methods


# Start plot methods
#' @export
setGeneric("plotLeaflet", function(object, save_im=FALSE, path = "") {
  standardGeneric("plotLeaflet")
})

setMethod(f = "plotLeaflet",
          signature= "sites",
          definition = function(object, save_im=FALSE, path = ""){
            df1 <- map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@location)[,2]),
                               long = mean(st_coordinates(x@location)[,1]),
                               elev = x@altitude,
                               description = x@description)
              
            }) %>%
              bind_rows()

            map1 <- leaflet(df1) %>% 
              addProviderTiles(providers$Stamen.TerrainBackground) %>% 
              addTiles() %>%
              addCircleMarkers(lng = df1$long, lat = df1$lat,
                               popup = paste0('<b>', df1$sitename, '</b><br><b>Description:</b> ', df1$description, '<br><a href=http://apps.neotomadb.org/explorer/?siteids=',df1$siteid,'>Explorer Link</a>'),
                                     clusterOptions = markerClusterOptions(),
                                     options = markerOptions(riseOnHover = TRUE))
           
            
            if(save_im == TRUE){
              mapshot(map1, file = path)
            }
            map1
          })
# End plot methods

# Start writeCSV methods
setMethod(f = "write.csv",
          signature= "sites",
          definition = function(object, path){
            df1 <- map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@location)[,2]),
                               long = mean(st_coordinates(x@location)[,1]),
                               elev = x@altitude,
                               description = x@description)
              
            }) %>%
              bind_rows()
            
            write.csv(df1, path, row.names = FALSE)
            
          })

setMethod(f = "write.csv",
          signature= "datasets",
          definition = function(object, path){
            df1 <- map(object@datasets, function(x) {
              df <- data.frame(siteid = x@datasetid,
                               sitename = x@datasetname,
                               lat = mean(st_coordinates(x@location)[,2]),
                               long = mean(st_coordinates(x@location)[,1]),
                               type = x@datasettype)
              
            }) %>%
              bind_rows()
            
            write.csv(df1, path, row.names = FALSE)
            
          }
)

# TODO1: Remove this showDatasets and show Datasets table when showing sites
#' @export
setGeneric("showDatasets", function(object) {
  standardGeneric("showDatasets")
})

setMethod(f = "showDatasets",
          signature= "sites",
          definition = function(object){
            my_datasets <- c()
            for(i in 1:length(object@sites)){
              my_dataset <- object@sites[[i]]@collunits@collunits[[1]]@datasets@datasets[[1]]
              
              my_datasets <- append(my_datasets, my_dataset)
              my_datasets2 <- new('datasets', datasets = my_datasets)
              
            }
            return(my_datasets2)
          })
# Finish TODO1
