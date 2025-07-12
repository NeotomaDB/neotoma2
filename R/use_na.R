#' @title Change NA values from logic to a prescribed type.
#' @description Pass an object and convert all
#' \code{NA} elements to particular \code{NA} types.
#' @param x An element that may or may not have NA values.
#' @param type A character string with values either \code{char} or \code{int}.
#' @returns object converted to `NA_character` or `NA_integer`
#' @keywords internal
#' @noRd
use_na <- function(x, type) {
  if (is.null(x) || is.na(x)) {
    return(switch(type,
                  "char" = NA_character_,
                  "int" = NA_integer_,
                  "sf" = st_as_sf(st_sfc()),
                  "list" = list()))
  } else {
    return(x)
  }
}