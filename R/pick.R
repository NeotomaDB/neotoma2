#' @title Pick the first field the API actually returned.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description The Neotoma API spells the same field differently depending on
#' which endpoint answered. A collection unit type, for example, arrives as
#' \code{collectionunittype} from \code{sites}, \code{unittype} from
#' \code{datasets} and \code{collunittype} from \code{downloads}. Pass every
#' spelling and \code{pick()} returns the value of the first one present.
#' @param x A named list from the API \code{data} section.
#' @param ... The candidate field names, in preference order.
#' @returns The value of the first field found in \code{x}, or \code{NULL} if
#'   none of them are present.
#' @keywords internal
#' @noRd
pick <- function(x, ...) {
  keys <- c(...)
  for (key in keys) {
    if (key %in% names(x) && !is.null(x[[key]])) {
      return(x[[key]])
    }
  }
  NULL
}
