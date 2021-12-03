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
                   geography = st_sf(st_sfc()),
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
                             elev = object@altitude), row.names = FALSE)
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
                               elev = x@altitude)
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

#' @title  $
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
            new("sites",
                sites = unlist(c(x@sites,
                                 y@sites), recursive = FALSE))
          })

#' @title write CSV
#' @param x sites object 1
#' @export
setMethod(f = "write.csv",
          signature = "sites",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })