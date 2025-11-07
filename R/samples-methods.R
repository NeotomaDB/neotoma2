#' @rdname sub-sub
#' @export
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

#' @rdname cash
#' @export
setMethod(f = "$",
          signature = signature(x = "samples"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @rdname cash
#' @export
setMethod(f = "$",
          signature = signature(x = "sample"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @rdname length
#' @export
setMethod(f = "length",
          signature = signature(x = "samples"),
          definition = function(x) {
            length(x@samples)
          })

#' @rdname c
#' @export
setMethod(f = "c",
          signature = signature(x = "samples"),
          definition = function(x, y) {
            print("im using this function")
            samp <- new("samples",
                        samples = unlist(c(x@samples,
                                           y@samples),
                                         recursive = FALSE))
            return(samp)
          })

#' @rdname sub-subset
#' @export
setMethod(f = "[[<-", 
          signature = signature(x = "samples"),
          definition = function(x, i, value) {
            samples <- x@samples
            samples[[i]] <- value
            out <- new("samples", sites = samples)
            return(out)
          })

#' @rdname subset
#' @export
setMethod(f = "[<-",
          signature = signature(x = "sample", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(length(i))) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })