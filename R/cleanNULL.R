#' @title Clean NULL values
#' @description Pass an object and convert all \code{NULL} elements to \code{NA}.
#' @param x An element that may or may not have NULL values.
#' @importFrom purrr map
# @NoRd
cleanNULL <- function(x) {
  if ('list' %in% class(x)) {
    out <- map(x, cleanNULL)
  } else {
    if (is.null(x)) {
      out <- NA
    } else { out <- x }
  }
  return (out)
}