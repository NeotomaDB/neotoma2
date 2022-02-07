#' An S4 class for site information from the Neotoma Paleoecology Database.
site <- setClass(
  # Set the name for the class
  "site",
  # Define the slots
  slots = c(siteid = "numeric",
            sitename = "character",
            geography = "sf",
            altitude = "numeric",
            geopolitical = "list",
            area = "numeric",
            notes = "character",
            description = "character",
            collunits = "collunits"),
  # Set the default values for the slot
  prototype = list(siteid = NA_integer_,
                   sitename = NA_character_,
                   geography = sf::st_sf(sf::st_sfc()),
                   geopolitical = list(),
                   altitude = NA_integer_,
                   area = NA_integer_,
                   notes = NA_character_,
                   description = NA_character_,
                   collunits = NULL) # check what would really be a NA here
  # Add a validity function that can test data consistency.
  # This is not called if you have an initialize function defined!
)

#' An S4 class for multi-site information from
#'  the Neotoma Paleoecology Database.
sites <- setClass("sites",
                  representation(sites = "list"),
                  validity = function(object) {
                    all(map(object@sites, function(x) {
                      class(x) == "site"
                    }) %>%
                      unlist())
                  })

#' @title Show Site objects as a dataframe
#' @param object site object
setMethod(f = "show",
          signature = "site",
          definition = function(object) {
            print(data.frame(siteid = object@siteid,
                             sitename = object@sitename,
                             lat = mean(st_coordinates(object@geography)[, 2]),
                             long = mean(st_coordinates(object@geography)[, 1]),
                             altitude = object@altitude), row.names = FALSE)
          })

#' @title Show Sites objects as a dataframe
#' @param object sites object
setMethod(f = "show",
          signature = "sites",
          definition = function(object) {
            map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@geography)[, 2]),
                               long = mean(st_coordinates(x@geography)[, 1]),
                               altitude = x@altitude)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @title Convert sites object to a \code{data.frame}
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @export
# Todo Convert to as.data.frame
setGeneric("showDatasets", 
           function(object) {
            standardGeneric("showDatasets")
           })

#' @title Convert sites object to a \code{data.frame}
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "showDatasets",
          signature = "sites",
          definition = function(object) {
            my_datasets <- c()
            for (i in seq_len(length(object@sites))) {
              collunits_call <- object@sites[[i]]@collunits@collunits[[1]]
              my_dataset <- collunits_call@datasets@datasets[[1]]
              my_datasets <- append(my_datasets, my_dataset)
              my_datasets2 <- new("datasets", datasets = my_datasets)
            }
            return(my_datasets2)
          })

#' @title  Slicer
#' @param x sites object
#' @param i iteration in sites list
#' @description Obtain one of the elements within a sites list
#' @export
setMethod(f = "[[",
          signature = signature(x = "sites", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("site", x@sites[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("site", x@sites[[z]])
              })
              out <- new("sites", sites = out)
            }
            return(out)
          })

#' @title Get site field by numeric index
#' @param x The site object
#' @param i The column indicator
#' @param drop
setMethod(f = "[",
          signature = signature(x = "site", i = "numeric"),
          definition = function(x, i, j, drop = FALSE) {
            slots <- slotNames(x)[i]
            as.data.frame(sapply(slots, function(y) slot(x, y)))
          })

#' @title Get site field by character index
#' @param x The site object
#' @param i The column indicator
#' @param drop
setMethod(f = "[",
          signature = signature(x = "site", i = "character"),
          definition = function(x, i, j, drop = FALSE) {
            out <- as.data.frame(lapply(i, function(y) slot(x, y)))
            colnames(out) <- i
            return(out)
          })

#' @title Get slot names
#' @param x
#' @description Get all names for named elements within a `site` object.
#' @export
setMethod(f = "names",
          signature = signature(x = "site"),
          definition = function(x) {
            slotNames(x)
          })

#' @title  Insert site
#' @param x sites object
#' @param i iteration in sites list
#' @param j 
#' @description Obtain one of the elements within a sites list
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "sites"),
          definition = function(x, i, j, value) {
            siteset <- x@sites
            siteset[[i]] <- value
            out <- new("sites", sites = siteset)
            return(out)
          })


#' @title Assign site field by numeric index
#' @param x The site object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @param drop
setMethod(f = "[<-",
          signature = signature(x = "site", i = "character"),
          definition = function(x, i, j, value, drop = FALSE) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @title Assign site field by numeric index
#' @param x The site object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @param drop
setMethod(f = "[<-",
          signature = signature(x = "site", i = "numeric"),
          definition = function(x, i, j, value, drop = FALSE) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title Assign site field by numeric index
#' @param x The site object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @param drop
setMethod(f = "$<-",
          signature = signature(x = "site"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })


#' @title $
#' @param x site object
#' @description Obtain slots of a site without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "site"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for sites
#' @param x sites object
#' @description Obtain slots of a site without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "sites"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @title  as.data.frame site
#' @param x site object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("site"),
          definition = function(x) {
            data.frame(siteid = x@siteid,
                       sitename = x@sitename,
                       lat = mean(st_coordinates(x@geography)[, 2]),
                       long = mean(st_coordinates(x@geography)[, 1]),
                       area = x@area,
                       notes = x@notes,
                       description = x@description,
                       elev = x@altitude)
          })

#' @title  as.data.frame sites
#' @param x sites object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("sites"),
          definition = function(x) {
            x@sites %>% map(as.data.frame) %>% bind_rows()
          })

#' @title  as.list sites
#' @param x sites object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.list",
          signature = signature("sites"),
          definition = function(x) {
            as.list(x@sites)
          })

#' @title Length Method Sites
#' @export
#' @param x sites object
setMethod(f = "length",
          signature = signature(x = "sites"),
          definition = function(x) {
            length(x@sites)
          })

#' @title c Method - Combine sites objects
#' @param x sites object 1
#' @param y sites object 2
#' @export
setMethod(f = "c",
          signature = signature(x = "sites"),
          definition = function(x, y) {
            if (class(y) == "sites") {
              out <- new("sites",
                         sites = unlist(c(x@sites,
                                          y@sites),
                                        recursive = FALSE))
            } else if (class(y) == "site") {
              siteset <- c(x@sites, y)
              out <- new("sites", sites = siteset)
            }
            return(out)
          })

#' @title write CSV
#' @param x sites object 1
#' @importFrom utils write.csv
#' @export
setMethod(f = "write.csv",
          signature = "sites",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
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

#' @title Return the latitude and logitude of sites
#' @param x sites object 1
#' @export
setMethod(f = "coordinates",
          signature = "sites",
          definition = function(obj, ...) {
            coords <- as.data.frame(obj)[, c("long", "lat")]
            return(coords)
          })

#' @title Plot site coordinates using a basic plot.
#' @param x sites object 1
#' @export
setMethod(f = "plot",
          signature = "sites",
          definition = function(x, ...) {
            coords <- as.data.frame(x)[, c("long", "lat")]
            plot(coords, ...)
          })

#' @title Summary of objects within a sites object.
#' @param object sites object
#' @param ... additional properties passed to \code{summary}
#' @importFrom dplyr bind_rows mutate select
#' @export
setMethod(f = "summary",
          signature = "sites",
          definition = function(object, ...) {
            sites <- sapply(object@sites, function(x) x@sitename)
            siteid <- sapply(object@sites, function(x) x@siteid)
            collunits <- lapply(object@sites, function(x) {
              datasets <- sapply(x@collunits@collunits, 
                                 function(y) length(y@datasets) )
              chronologies <- sapply(x@collunits@collunits, 
                                     function(y) length(y@chronologies) )
              return(data.frame(collunits = length(x@collunits),
                         datasets = datasets))
            })
            
            collunits %>% 
              bind_rows() %>% 
              mutate(sitename = sites, siteid = siteid) %>% 
              select(siteid, sitename, collunits, datasets)
          })
          
