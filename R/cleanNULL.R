#' @title Clean NULL values
#' @author Simon Goring \email{goring@wisc.edu}
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description Pass an object and convert all
#' \code{NULL} elements to \code{NA}.
#' @param x An element that may or may not have NULL values.
#' @returns parsed `list` where NULL values are changed to NA
#' @noRd
cleanNULL <- function(x) {
  if (is.list(x)) {
    lapply(x, cleanNULL)
  } else if (is.null(x)) {
    NA
  } else {
    x
  }
}