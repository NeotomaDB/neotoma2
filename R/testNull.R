#' @title Clear NULL values and replace with value predefined value.
#' @description Similar to concatenate, checks to see if a value is NULL
#' and replace it with a known value provided in \code{out}.
#' @param val The value to be checked.
#' @param out A default value to be returned if \code{val} is NULL.
#' @returns The values passed in \code{val} or \code{out} if
#'   \code{val} is \code{NULL}.
#' @examples {
#' # Passing a null value into the function returns 12:
#' a <- testNull(val = NULL, out = 12)
#' # Passing a non-NULL value returns that value:
#' b <- testNull(val = 11, out = 12)
#' }
#' @keywords internal
#' @noRd
testNull <- function(val, out=NA) {
  if (is.null(val)) {
    return(out)
    } else {
      return(val)
    }
}