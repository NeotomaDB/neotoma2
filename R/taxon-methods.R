#' @rdname show
#' @export
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

#' @rdname show
#' @export
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

#' @rdname sub-sub
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

#' @rdname names
#' @export
setMethod(f = "names",
          signature = signature(x = "taxon"),
          definition = function(x) {
            slotNames(x)
          })

#' @rdname sub-subset
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "taxa"),
          definition = function(x, i, value) {
            taxaset <- x@taxa
            taxaset[[i]] <- value
            out <- new("taxa", taxa = taxaset)
            return(out)
          })

#' @rdname subset
#' @export
setMethod(f = "[<-",
          signature = signature(x = "taxon", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @rdname subset
#' @export
setMethod(f = "[<-",
          signature = signature(x = "taxon", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in seq_along(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @rdname cash-set
#' @export
setMethod(f = "$<-",
          signature = signature(x = "taxon"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @rdname sub
#' @export
setMethod(f = "[",
          signature = signature(x = "taxa", i = "numeric"),
          definition = function(x, i) {
            new("taxa", taxa = x@taxa[i])
          })

#' @rdname cash
#' @export
setMethod(f = "$",
          signature = signature(x = "taxon"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @rdname cash
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

#' @rdname as.data.frame
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

#' @rdname as.data.frame
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

#' @rdname length
#' @export
setMethod(f = "length",
          signature = signature(x = "taxa"),
          definition = function(x) {
            length(x@taxa)
          })

#' @rdname c
#' @export
setMethod(f = "c",
          signature = signature(x = "taxa"),
          definition = function(x, y) {
            new("taxa",
                taxa = unlist(c(x@taxa,
                                y@taxa),
                              recursive = FALSE))
          })