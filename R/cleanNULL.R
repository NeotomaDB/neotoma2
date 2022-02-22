#' @title Clean NULL values
#' @description Pass an object and convert all
#' \code{NULL} elements to \code{NA}.
#' @param x An element that may or may not have NULL values.
#' @export
#' @noRd
cleanNULL <- function(x) { # nolint
  out <- rapply(x,
                function(y) { ifelse(is.null(y), NA, y) },
                how = "replace")
  return(out)
}
