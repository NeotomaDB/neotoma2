#' @title Change NA values from logic to a prescribed type.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @description Pass an object and convert all
#' \code{NA} elements to particular \code{NA} types.
#' @param x An element that may or may not have NA values.
#' @param type A character string with values either \code{char} or \code{int}.
#' @returns object converted to `NA_character` or `NA_integer`
#' @keywords internal
#' @noRd
use_na <- function(x, type) {
  tryCatch({
    if (type == "sf") {
      if (is.null(x) || nrow(x) == 0) {
        return(sf::st_as_sf(sf::st_sfc()))
      } else {
        return(x)
      }
    }
    if (is.null(x) || (is.atomic(x) && all(is.na(x)))) {
      return(switch(type,
                    "char" = NA_character_,
                    "int"  = NA_integer_,
                    "list" = list(),
                    "date" = as.Date(NA_character_),
                    "bool" = NA,
                    "df"   = data.frame(),
                    NA))
    }
    x
  }, error = function(e) {
    # Fallback
    switch(type,
           "char" = NA_character_,
           "int"  = NA_integer_,
           "sf"   = sf::st_as_sf(sf::st_sfc()),
           "list" = list(),
           "date" = as.Date(NA_character_),
           "bool" = NA,
           "df"   = data.frame(),
           NA)
  })
}