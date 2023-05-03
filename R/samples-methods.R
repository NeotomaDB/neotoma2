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
