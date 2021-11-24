#' @title Clean NULL values
#' @description Pass an object and convert all
#' \code{NULL} elements to \code{NA}.
#' @param x An element that may or may not have NULL values.
#' @importFrom purrr map
#' @noRd
cleanNULL <- function(x) { # nolint
  if ("list" %in% class(x)) {
    out <- purrr::map(x, cleanNULL)
  } else {
    if (is.null(x)) {
      out <- NA
    } else {
      out <- x
      }
  }
  return(out)
}