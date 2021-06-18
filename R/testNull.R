#' @param val A value to be passed in
#' @param out A default value to be returned if \code{val} is NULL.
#' @noRd
testNull <- function(val, out) {
  if(is.null(val)) { return(out)} else {return(val)}
}