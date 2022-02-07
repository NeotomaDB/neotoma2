# Start "c" methods
#' @title c Method - Combine objects, including NULL
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @title c Method for NULL values
#' @param x NULL object
#' @param y sites/collunits object
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x ="missingORNULL", y) {
            y
          })

#' @title S4 class for collunits information
collunit <- setClass(
  # Set the name for the class
  "collunit",
  slots = c(collectionunitid = "numeric",
            notes = "character",
            handle = "character",
            colldate = "Date",
            location = "character",
            waterdepth = "numeric",
            gpslocation = "sf",
            collunittype = "character",
            collectiondevice = "character",
            collectionunitname = "character",
            depositionalenvironment = "character",
            datasets = "datasets",
            chronologies = "chronologies"),
  prototype = list(collectionunitid = NA_integer_,
                   notes = NA_character_,
                   handle = NA_character_,
                   colldate = as.Date(character(0)),
                   location = NA_character_,
                   waterdepth = NA_integer_,
                   gpslocation = sf::st_as_sf(sf::st_sfc()),
                   collunittype = NA_character_,
                   collectiondevice = NA_character_,
                   collectionunitname = NA_character_,
                   depositionalenvironment = NA_character_,
                   datasets = NULL,
                   chronologies = NULL))

#' An S4 class for Neotoma Collection Units
#' @description Holds Collection unit information
#'  from the Neotoma Paleoecology Database.
#' @importFrom purrr map
collunits <- setClass("collunits",
                      representation(collunits = "list"),
                      validity = function(object) {
                        all(map(object@collunits,
                        function(x) {
                          class(x) == "collunit"
                        }) %>%
                        unlist())
                      })

# Show methods go here

#' @title  Slicer
#' @param x collunits object
#' @param i iteration in collunits list
#' @description Obtain one of the elements within a collunits list
#' @export
setMethod(f = "[[",
          signature = signature(x = "collunits", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("collunit", x@collunits[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("collunit", x@collunits[[z]])
              })
              out <- new("collunits", collunits = out)
            }
            return(out)
          })

#' @title Get slot names
#' @param x
#' @description Get all names for named elements within a `collunit` object.
#' @export
setMethod(f = "names",
          signature = signature(x = "collunit"),
          definition = function(x) {
            slotNames(x)
          })

#' @title  Insert collunit
#' @param x collunits object
#' @param i iteration in collunits list
#' @param j 
#' @description Obtain one of the elements within a collunits list
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "collunits"),
          definition = function(x, i, j, value) {
            collunitset <- x@collunits
            collunitset[[i]] <- value
            out <- new("collunits", collunits = collunitset)
            return(out)
          })


#' @title Assign collunit field by numeric index
#' @param x The collunit object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @param drop
setMethod(f = "[<-",
          signature = signature(x = "collunit", i = "character"),
          definition = function(x, i, j, value, drop = FALSE) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @title Assign collunit field by numeric index
#' @param x The collunit object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @param drop
setMethod(f = "[<-",
          signature = signature(x = "collunit", i = "numeric"),
          definition = function(x, i, j, value, drop = FALSE) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title Assign collunit field by numeric index
#' @param x The collunit object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @param drop
setMethod(f = "$<-",
          signature = signature(x = "collunit"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })


#' @title  $
#' @param x collunit object
#' @description Obtain slots of a collunit without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "collunit"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for collunits
#' @param x collunits object
#' @description Obtain slots of a collunit without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "collunits"),
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
          signature = signature("collunit"),
          definition = function(x) {
            data.frame(collectionunitid = object@collectionunit,
                       notes = object@notes,
                       handle = object@handle,
                       colldate = object@colldate,
                       location = object@location,
                       waterdepth = object@waterdepth,
                       gpslocation = object@gpslocation,
                       collunittype = object@collunittype,
                       collectiondevice = object@collectiondevice,
                       collectionunitname = object@collectionunitname,
                       depositionalenvironment = object@depositionalenvironment)
          })

#' @title  as.data.frame collunits
#' @param x collunits object
#' @description show as dataframe as prep to save as csv
#' @export
setMethod(f = "as.data.frame",
          signature = signature("collunits"),
          definition = function(x) {
            x@collunits %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method collunits
#' @export
#' @param x collunits object
setMethod(f = "length",
          signature = signature(x = "collunits"),
          definition = function(x) {
            length(x@collunits)
          })

#' @title c Method - Combine collunits objects
#' @param x collunits object 1
#' @param y collunits object 2
#' @export
setMethod(f = "c",
          signature = signature(x = "collunits"),
          definition = function(x, y) {
            new("collunits",
                collunits = unlist(c(x@collunits,
                                    y@collunits), recursive = FALSE))
          })

#' @title write CSV
#' @param x collunits object
#' @export
setMethod(f = "write.csv",
          signature = "collunits",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })
