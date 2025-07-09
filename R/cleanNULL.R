#' @title Clean NULL values
#' @description Pass an object and convert all
#' \code{NULL} elements to \code{NA}.
#' @param x An element that may or may not have NULL values.
#' @returns parsed `list` where NULL values are changed to NA
#' @keywords internal
#' @noRd
cleanNULL <- function(x) {
  out <- rapply(x,
                function(y) {
                  ifelse(is.null(y), NA, y)
                },
                how = "replace")
  return(out)
}
