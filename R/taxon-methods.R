#' @aliases show,taxon-method
#' @rdname show
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
                             publication = object@publication),
                  row.names = FALSE)
          })

#' @aliases show,taxa-method
#' @rdname show
setMethod(f = "show",
          signature = "taxa",
          definition = function(object) {
            map(object@taxa, function(y) {
              data.frame(taxonid = as.character(y@taxonid),
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

#' @aliases sub-sub,taxa-method
#' @rdname sub-sub
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

#' @aliases names,taxon-method
#' @rdname names
setMethod(f = "names",
          signature = signature(x = "taxon"),
          definition = function(x) {
            slotNames(x)
          })

#' @aliases sub-subset,taxa-method
#' @rdname sub-subset
setMethod(f = "[[<-",
          signature = signature(x = "taxa"),
          definition = function(x, i, value) {
            taxaset <- x@taxa
            taxaset[[i]] <- value
            out <- new("taxa", taxa = taxaset)
            return(out)
          })

#' @aliases subset,taxon-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "taxon", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @aliases subset,taxon-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "taxon", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in seq_along(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @aliases cash-set,taxon-method
#' @rdname cash-set
setMethod(f = "$<-",
          signature = signature(x = "taxon"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @aliases sub,taxa-method
#' @rdname sub
setMethod(f = "[",
          signature = signature(x = "taxa", i = "numeric"),
          definition = function(x, i) {
            new("taxa", taxa = x@taxa[i])
          })

#' @aliases cash,taxon-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "taxon"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @aliases cash,taxa-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "taxa"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @aliases as.data.frame,taxon-method
#' @rdname as.data.frame
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

#' @aliases as.data.frame,taxa-method
#' @rdname as.data.frame
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

#' @aliases length,taxa-method
#' @rdname length
setMethod(f = "length",
          signature = signature(x = "taxa"),
          definition = function(x) {
            length(x@taxa)
          })

#' @aliases c,taxa-method
#' @rdname c
setMethod(f = "c",
          signature = signature(x = "taxa"),
          definition = function(x, y) {
            new("taxa",
                taxa = unlist(c(x@taxa,
                                y@taxa),
                              recursive = FALSE))
          })