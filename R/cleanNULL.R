#' @title Clean NULL values
#' @description Pass an object and convert all
#' \code{NULL} elements to \code{NA}.
#' @param x An element that may or may not have NULL values.
#' @returns parsed `list` where NULL values are changed to NA
#' @keywords internal
#' @noRd
cleanNULL <- function(x) {
  if (is.list(x)) {
    lapply(x, function(el) {
      if (is.null(el)) {
        NA
      } else {
        cleanNULL(el)
      }
    })
  } else {
    x
  }
}