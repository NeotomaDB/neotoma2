#' @aliases sub-sub,samples-method
#' @rdname sub-sub
setMethod(f = "[[",
          signature = signature(x = "samples", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("sample", x@samples[[i]])
            } else {
              out <- map(i, function(z) {
                new("sample", x@samples[[z]])
              })
              out <- new("samples", samples = out)
            }
            return(out)
          })

#' @aliases cash,samples-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "samples"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @aliases cash,sample-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "sample"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @aliases length,samples-method
#' @rdname length
setMethod(f = "length",
          signature = signature(x = "samples"),
          definition = function(x) {
            length(x@samples)
          })

#' @aliases c,samples-method
#' @rdname c
setMethod(f = "c",
          signature = signature(x = "samples"),
          definition = function(x, y) {
            samp <- new("samples",
                        samples = unlist(c(x@samples,
                                           y@samples),
                                         recursive = FALSE))
            return(samp)
          })

#' @aliases sub-subset,samples-method
#' @rdname sub-subset
setMethod(f = "[[<-", 
          signature = signature(x = "samples"),
          definition = function(x, i, value) {
            samples <- x@samples
            samples[[i]] <- value
            out <- new("samples", sites = samples)
            return(out)
          })

#' @aliases subset,sample-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "sample", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(length(i))) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })