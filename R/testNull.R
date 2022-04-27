#' @title Clear out NULL values and replace with value
#' @description Similar to concatenate, checks to see if a value is NULL
#' and replace it with a known value.
#' @param val A value to be passed in
#' @param out A default value to be returned if \code{val} is NULL.
#' @return The values passed in \code{val} or \code{out} if \code{val} is \code{NULL}.
#' @export
testNull <- function(val, out=NA) { # nolint
  if (is.null(val)) {
    return(out)
    } else {
      return(val)
    }
}
