#' @title Fix null values from API responses
#' @description API responses from the Neotoma API return many fields
#' as NULL values. This function turns the NULL values into logical NAs.
#' @param x item that has null objects.
#' @returns `list` with `NA`s instead of `null` objects
#' @keywords internal
#' @noRd
fix_null <- function(x) {
    for (i in seq_len(length(x))) {
      if (is.null(x[[i]])) {
        x[[i]] <- NA
      } else {
        if (is(x[[i]], "list")) {
          x[[i]] <- fix_null(x[[i]])
        }
      }
    }
    return(x)
  }
