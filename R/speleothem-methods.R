#' @aliases show,speleothem-method
#' @rdname show
setMethod(f = "show",
          signature = "speleothem",
          definition = function(object) {
            print(data.frame(entityid = object@entityid,
                    entityname = object@entityname,
                    siteid = object@siteid,
                    collectionunitid = object@collectionunitid,
                    speleothemdriptype = object@speleothemdriptype,
                    dripheight = object@dripheight,
                    dripheightunits = object@dripheightunits,
                    monitoring = object@monitoring,
                    geology = object@geology,
                    relativeage = object@relativeage,
                    speleothemtype = object@speleothemtype,
                    entrancedistance = object@entrancedistance,
                    entrancedistanceunits = object@entrancedistanceunits,
                    landusecovertype = object@landusecovertype,
                    landusecoverpercent = object@landusecoverpercent,
                    vegetationcovertype = object@vegetationcovertype,
                    vegetationcoverpercent = object@vegetationcoverpercent,
                    entitycovertype = object@entitycovertype,
                    entitycoverthickness = object@entitycoverthickness
                  ), row.names = FALSE)
          })

#' @aliases show,speleothems-method
#' @rdname show
setMethod(f = "show",
          signature = "speleothems",
          definition = function(object) {
            map(object, function(y) {
              data.frame(entityid = y@entityid,
                         entityname = y@entityname,
                         siteid = y@siteid,
                         collectionunitid = y@collectionunitid,
                         speleothemdriptype = y@speleothemdriptype,
                         dripheight = y@dripheight,
                         dripheightunits = y@dripheightunits,
                         monitoring = y@monitoring,
                         geology = y@geology,
                         relativeage = y@relativeage,
                         speleothemtype = y@speleothemtype,
                         entrancedistance = y@entrancedistance,
                         entrancedistanceunits = y@entrancedistanceunits,
                         landusecovertype = y@landusecovertype,
                         landusecoverpercent = y@landusecoverpercent,
                         vegetationcovertype = y@vegetationcovertype,
                         vegetationcoverpercent = y@vegetationcoverpercent,
                         entitycovertype = y@entitycovertype,
                         entitycoverthickness = y@entitycoverthickness)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @aliases sub-sub,speleothems-method
#' @rdname sub-sub
setMethod(f = "[[",
          signature = signature(x = "speleothems", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("speleothem", x@speleothems[[i]])
            } else {
              out <- map(i, function(z) {
                new("dataset", x@speleothems[[z]])
              })
              out <- new("speleothems", speleothems = out)
            }
            return(out)
          })

#' @aliases names,speleothem-method
#' @rdname names
setMethod(f = "names",
          signature = signature(x = "speleothem"),
          definition = function(x) {
            slotNames(x)
          })

#' @aliases sub-subset,speleothems-method
#' @rdname sub-subset
setMethod(f = "[[<-",
          signature = signature(x = "speleothems"),
          definition = function(x, i, value) {
            speleothemset <- x@speleothems
            speleothemset[[i]] <- value
            out <- new("speleothems", speleothems = speleothemset)
            return(out)
          })

#' @aliases subset,speleothem-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "speleothem", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @aliases subset,speleothem-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "speleothem", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in seq_along(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @aliases cash-set,speleothem-method
#' @rdname cash-set
setMethod(f = "$<-",
          signature = signature(x = "speleothem"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @aliases sub,speleothems-method
#' @rdname sub
setMethod(f = "[",
          signature = signature(x = "speleothems", i = "numeric"),
          definition = function(x, i) {
            new("speleothems", speleothems = x@speleothems[i])
          })

#' @aliases cash,speleothem-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "speleothem"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @aliases cash,speleothems-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "speleothems"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @aliases as.data.frame,speleothem-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("speleothem"),
          definition = function(x) {
            data.frame(entityid = x@entityid,
                       entityname = x@entityname,
                       siteid = x@siteid,
                       collectionunitid = x@collectionunitid,
                       speleothemdriptype = x@speleothemdriptype,
                       dripheight = x@dripheight,
                       dripheightunits = x@dripheightunits,
                       monitoring = x@monitoring,
                       geology = x@geology,
                       relativeage = x@relativeage,
                       speleothemtype = x@speleothemtype,
                       entrancedistance = x@entrancedistance,
                       entrancedistanceunits = x@entrancedistanceunits,
                       landusecovertype = x@landusecovertype,
                       landusecoverpercent = x@landusecoverpercent,
                       vegetationcovertype = x@vegetationcovertype,
                       vegetationcoverpercent = x@vegetationcoverpercent,
                       entitycoverthickness = x@entitycoverthickness,
                       entitycovertype = x@entitycovertype)
          })

#' @aliases as.data.frame,speleothems-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("speleothems"),
          definition = function(x) {
            x@speleothems %>% map(as.data.frame) %>% bind_rows()
          })

#' @aliases length,speleothems-method
#' @rdname length
setMethod(f = "length",
          signature = signature(x = "speleothems"),
          definition = function(x) {
            length(x@speleothems)
          })

#' @aliases c,speleothems-method
#' @rdname c
setMethod(f = "c",
          signature = signature(x = "speleothems"),
          definition = function(x, y) {
            new("speleothems",
                speleothems = unlist(c(x@speleothems,
                                       y@speleothems),
                                     recursive = FALSE))
          })