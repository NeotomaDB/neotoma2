# Start "Show Method" for all Neotoma Objects
#' @title Show Taxon Method
#' @param object taxon object
#' @returns null - side effect, prints a `data.frame` with `taxon` metadata
setMethod(f = "show",
          signature = "taxon",
          definition = function(object) {
            print(data.frame(taxonid = as.character(object@taxonid),
                             taxoncode = object@taxoncode,
                             taxonname = object@taxonname,
                             author =  object@author,
                             ecolgroup =  object@ecolgroup,
                             highertaxonid = object@highertaxonid,
                             status = object@status,
                             taxagroupid = object@taxagroupid,
                             publicationid = object@publicationid,
                             publication = object@publication), row.names = FALSE)
          })

# Start "Show Method" for all Neotoma Objects
#' @title Show Taxa Method
#' @param object taxon object
#' @returns null - side effect, prints a `data.frame` with `taxon` metadata
setMethod(f = "show",
          signature = "taxa",
          definition = function(object) {
            map(object@taxa, function(y) {
              df <- data.frame(taxonid = as.character(y@taxonid),
                               taxoncode = y@taxoncode,
                               taxonname = y@taxonname,
                               author =  y@author,
                               ecolgroup =  y@ecolgroup,
                               highertaxonid = y@highertaxonid,
                               status = y@status,
                               taxagroupid = y@taxagroupid,
                               publicationid = y@publicationid,
                               publication = y@publication)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })


#' @title  Slicer
#' @param x taxa object
#' @param i iteration in taxa list
#' @description Obtain one of the elements within a taxa list
#' @returns sliced `taxa` object
#' @export
setMethod(f = "[[",
          signature = signature(x = "taxa", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("taxon", x@taxa[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("taxon", x@taxa[[z]])
              })
              out <- new("taxa", taxa = out)
            }
            return(out)
          })

#' @title Get slot names
#' @param x A taxon object.
#' @description Get all names for named elements within a `taxon` object.
#' @returns `list` with all names of `taxon` slots
#' @export
setMethod(f = "names",
          signature = signature(x = "taxon"),
          definition = function(x) {
            slotNames(x)
          })

#' @title  Insert taxon
#' @param x taxa object
#' @param i iteration in taxa list
#' @param value The value to be used
#' @description Obtain one of the elements within a taxa list
#' @returns One `taxon` slot's value 
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "taxa"),
          definition = function(x, i, value) {
            taxaset <- x@taxa
            taxaset[[i]] <- value
            out <- new("taxa", taxa = taxaset)
            return(out)
          })


#' @title Assign taxon field by numeric index
#' @param x The taxon object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `taxon` slot with new assigned character value
setMethod(f = "[<-",
          signature = signature(x = "taxon", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @title Assign taxon field by numeric index
#' @param x The taxon object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `taxon` slot with new assigned numeric value
setMethod(f = "[<-",
          signature = signature(x = "taxon", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title Assign taxon field by numeric index
#' @param x The taxon object.
#' @param name name of the slot.
#' @param value The value to be used.
#' @returns Assign new `taxon` by numeric index
setMethod(f = "$<-",
          signature = signature(x = "taxon"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })



#' @title Get or remove taxa by numeric index
#' @param x The taxa object
#' @param i The numeric index
#' @returns Get or remove `taxa` by numeric index
setMethod(f = "[",
          signature = signature(x = "taxa", i = "numeric"),
          definition = function(x, i) {
            new("taxa", taxa = x@taxa[i])
          })

#' @title  $
#' @param x taxon object
#' @param name name of the slot
#' @description Obtain slots of a taxon without using at-mark
#' @returns Obtain a `taxon`'s `slot` value using $
#' @export
setMethod(f = "$",
          signature = signature(x = "taxon"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for taxa
#' @param x taxa object
#' @param name name of the slot.
#' @description Obtain slots of a taxon without using at-mark
#' @returns Obtain a `taxa`' `slot` value using $  
#' @export
setMethod(f = "$",
          signature = signature(x = "taxa"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @title  as.data.frame taxon
#' @param x taxon object
#' @description show as dataframe as prep to save as csv
#' @returns `data.frame` with `taxon` metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature("taxon"),
          definition = function(x) {
            data.frame(taxonid = as.character(x@taxonid),
                       taxoncode = x@taxoncode,
                       taxonname = x@taxonname,
                       author =  x@author,
                       ecolgroup =  x@ecolgroup,
                       highertaxonid = x@highertaxonid,
                       status = x@status,
                       taxagroupid = x@taxagroupid,
                       publicationid = x@publicationid,
                       publication = x@publication)
          })

#' @title  as.data.frame taxa
#' @param x taxa object
#' @description show as dataframe as prep to save as csv
#' @returns `data.frame` with `taxa` metadata
#' @export
setMethod(f = "as.data.frame",
          signature = "taxa",
          definition = function(x) {
            df <- map(x@taxa, function(y) {
              data.frame(
                taxonid = as.character(y@taxonid),
                taxoncode = y@taxoncode,
                taxonname = y@taxonname,
                author = y@author,
                ecolgroup = y@ecolgroup,
                highertaxonid = y@highertaxonid,
                status = y@status,
                taxagroupid = y@taxagroupid,
                publicationid = y@publicationid,
                publication = y@publication
              )
            }) %>%
              bind_rows()
            
            return(df)
          }
)

#' @title Length Method taxa
#' @export
#' @returns `int` that showcases the length of a `taxa` object
#' @param x taxa object
setMethod(f = "length",
          signature = signature(x = "taxa"),
          definition = function(x) {
            length(x@taxa)
          })

#' @title c Method - Combine taxa objects
#' @param x taxa object 1
#' @param y taxa object 2
#' @returns concatenated `taxa` object
#' @export
setMethod(f = "c",
          signature = signature(x = "taxa"),
          definition = function(x, y) {
            new("taxa",
                taxa = unlist(c(x@taxa,
                                    y@taxa), recursive = FALSE))
          })

#' @title write CSV
#' @param x taxa object
#' @param ... Additional parameters associated with the call.
#' @returns null -side effect for printing a CSV file
#' @export
setMethod(f = "write.csv",
          signature = "taxa",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })
