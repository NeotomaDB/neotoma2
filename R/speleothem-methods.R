# Start "Show Method" for all Neotoma Objects
#' @title Show Speleothem Method
#' @param object speleothem object
#' @returns null - side effect, prints a `data.frame` with `speleo` metadata
setMethod(f = "show",
          signature = "speleothem",
          definition = function(object) {
            print(data.frame(entityid = object@entityid,
                             entityname = object@entityname,
                             siteid = object@siteid,
                             collectionunitid = object@collectionunitid,
                             dripheight = object@dripheight,
                             monitoring = object@monitoring,
                             relativeage = object@relativeage,
                             speleothemtype = object@speleothemtype,
                             dripheightunits = object@dripheightunits,
                             entitycovertype = object@entitycovertype,
                             entrancedistance = object@entrancedistance,
                             landusecovertype = object@landusecovertype,
                             speleothemdriptype = object@speleothemdriptype,
                             landusecoverpercent = object@landusecoverpercent,
                             vegetationcovertype = object@vegetationcovertype,
                             entitycoverthickness = object@entitycoverthickness,
                             entrancedistanceunits = object@entrancedistanceunits,
                             vegetationcoverpercent = object@vegetationcoverpercent), row.names = FALSE)
          })

#' @title Show Datasets object as a dataframe
#' @param object datasets object
#' @returns null - side effect, prints a `data.frame` with `datasets` metadata
setMethod(f = "show",
          signature = "speleothems",
          definition = function(object) {
            map(object, function(y) {
              df <- data.frame(entityid = y@entityid,
                               entityname = y@entityname,
                               siteid = y@siteid,
                               collectionunitid = y@collectionunitid,
                               dripheight = y@dripheight,
                               monitoring = y@monitoring,
                               relativeage = y@relativeage,
                               speleothemtype = y@speleothemtype,
                               dripheightunits = y@dripheightunits,
                               entitycovertype = y@entitycovertype,
                               entrancedistance = y@entrancedistance,
                               landusecovertype = y@landusecovertype,
                               speleothemdriptype = y@speleothemdriptype,
                               landusecoverpercent = y@landusecoverpercent,
                               vegetationcovertype = y@vegetationcovertype,
                               entitycoverthickness = y@entitycoverthickness,
                               entrancedistanceunits = y@entrancedistanceunits,
                               vegetationcoverpercent = y@vegetationcoverpercent)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @title  Slicer
#' @param x speleothems object
#' @param i iteration in speleothems list
#' @description Obtain one of the elements within a datasets list
#' @returns sliced `dataset` object
#' @export
setMethod(f = "[[",
          signature = signature(x = "speleothems", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("speleothem", x@speleothems[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("dataset", x@speleothems[[z]])
              })
              out <- new("speleothems", speleothems = out)
            }
            return(out)
          })

#' @title Get slot names
#' @param x A dataset object.
#' @description Get all names for named elements within a `dataset` object.
#' @returns `list` with all names of `dataset` slots
#' @export
setMethod(f = "names",
          signature = signature(x = "speleothem"),
          definition = function(x) {
            slotNames(x)
          })

#' @title  Insert dataset
#' @param x speleothems object
#' @param i iteration in speleothems list
#' @param value The value to be used
#' @description Obtain one of the elements within a speleothems list
#' @returns One `speleothem` slot's value 
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "speleothems"),
          definition = function(x, i, value) {
            speleothemset <- x@speleothems
            speleothemset[[i]] <- value
            out <- new("speleothems", speleothems = speleothemset)
            return(out)
          })


#' @title Assign speleothem field by numeric index
#' @param x The speleothem object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `speleothem` slot with new assigned character value
setMethod(f = "[<-",
          signature = signature(x = "speleothem", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @title Assign speleothem field by numeric index
#' @param x The speleothem object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `speleothem` slot with new assigned numeric value
setMethod(f = "[<-",
          signature = signature(x = "speleothem", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title Assign speleothem field by numeric index
#' @param x The speleothem object.
#' @param name name of the slot.
#' @param value The value to be used.
#' @returns Assign new `speleothem` by numeric index
setMethod(f = "$<-",
          signature = signature(x = "speleothem"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })



#' @title Get or remove speleothems by numeric index
#' @param x The speleothems object
#' @param i The numeric index
#' @returns Get or remove `speleothems` by numeric index
setMethod(f = "[",
          signature = signature(x = "speleothems", i = "numeric"),
          definition = function(x, i) {
            new("speleothems", speleothems = x@speleothems[i])
          })

#' @title  $
#' @param x speleothem object
#' @param name name of the slot
#' @description Obtain slots of a speleothem without using at-mark
#' @returns Obtain a `speleothem`'s `slot` value using $
#' @export
setMethod(f = "$",
          signature = signature(x = "speleothem"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for speleothems
#' @param x speleothems object
#' @param name name of the slot.
#' @description Obtain slots of a speleothem without using at-mark
#' @returns Obtain a `speleothems`' `slot` value using $  
#' @export
setMethod(f = "$",
          signature = signature(x = "speleothems"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @title  as.data.frame speleothem
#' @param x speleothem object
#' @description show as dataframe as prep to save as csv
#' @returns `data.frame` with `speleothem` metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature("speleothem"),
          definition = function(x) {
            data.frame(entityid = x@entityid,
                       entityname = x@entityname,
                       siteid = x@siteid,
                       collectionunitid = x@collectionunitid,
                       dripheight = x@dripheight,
                       monitoring = x@monitoring,
                       relativeage = x@relativeage,
                       speleothemtype = x@speleothemtype,
                       dripheightunits = x@dripheightunits,
                       entitycovertype = x@entitycovertype,
                       entrancedistance = x@entrancedistance,
                       landusecovertype = x@landusecovertype,
                       speleothemdriptype = x@speleothemdriptype,
                       landusecoverpercent = x@landusecoverpercent,
                       vegetationcovertype = x@vegetationcovertype,
                       entitycoverthickness = x@entitycoverthickness,
                       entrancedistanceunits = x@entrancedistanceunits,
                       vegetationcoverpercent = x@vegetationcoverpercent)
          })

#' @title  as.data.frame speleothems
#' @param x speleothems object
#' @description show as dataframe as prep to save as csv
#' @returns `data.frame` with `speleothems` metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature("speleothems"),
          definition = function(x) {
            x@speleothems %>% map(as.data.frame) %>% bind_rows()
          })

#' @title Length Method speleothems
#' @export
#' @returns `int` that showcases the length of a `speleothems` object
#' @param x speleothems object
setMethod(f = "length",
          signature = signature(x = "speleothems"),
          definition = function(x) {
            length(x@speleothems)
          })

#' @title c Method - Combine speleothems objects
#' @param x speleothems object 1
#' @param y speleothems object 2
#' @returns concatenated `speleothems` object
#' @export
setMethod(f = "c",
          signature = signature(x = "speleothems"),
          definition = function(x, y) {
            new("speleothems",
                speleothems = unlist(c(x@speleothems,
                                    y@speleothems), recursive = FALSE))
          })