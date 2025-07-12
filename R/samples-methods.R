#' @title  Slicer
#' @param x samples object
#' @param i iteration in samples list
#' @description Obtain one of the elements within a samples list
#' @returns `samples` sliced object
#' @export
setMethod(f = "[[",
          signature = signature(x = "samples", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("sample", x@samples[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("sample", x@samples[[z]])
              })
              out <- new("samples", samples = out)
            }
            return(out)
          })

#' @title  $
#' @param x sample object
#' @param name name of the slot
#' @description Obtain slots of a sample without using at-mark
#' @returns `value` at selected slot
#' @export
setMethod(f = "$",
          signature = signature(x = "sample"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for samples
#' @param x samples object
#' @param name name of the slot
#' @description Obtain slots of a site without using at-mark
#' @returns `value` at selected slot
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


#' @title Length Method samples
#' @export
#' @param x samples object
#' @returns `int` representing the length of `samples object`
setMethod(f = "length",
          signature = signature(x = "samples"),
          definition = function(x) {
            length(x@samples)
          })

#' @title c Method - Combine datasets objects
#' @param x datasets object 1
#' @param y datasets object 2
#' @returns concatenated `samples` object
#' @export
setMethod(f = "c",
          signature = signature(x = "samples"),
          definition = function(x, y) {
            print("im using this function")
            samp <- new("samples",
                samples = unlist(c(x@samples,
                                    y@samples), recursive = FALSE))
            return(samp)
          })

#' @title  Insert sample
#' @param x samples object
#' @param i iteration in samples list
#' @param value The value to be used
#' @description Obtain one of the elements within a samples list
#' @returns `samples` object with reassigned values
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "samples"),
          definition = function(x, i, value) {
            samples <- x@samples
            samples[[i]] <- value
            out <- new("samples", sites = samples)
            return(out)
          })

#' @title Assign sample field by numeric index
#' @param x The sample object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `sample` object with reassigned character values
setMethod(f = "[<-",
          signature = signature(x = "sample", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(length(i))) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })
