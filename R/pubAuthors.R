#' @title Reformat API author list to Neotoma contacts.
#' @param x A list coming from the `author` element of
#'  the Neotoma publicaiton API
#' @importFrom purrr map
#' @returns `author` object
#' @noRd
pubAuthors <- function(x) { # nolint

  result <- new("authors",
                authors = map(x$author, function(y) {
                  y[is.null(y)] <- NA_character_
                  y[is.na(y)] <- NA_character_
                  new("author",
                      author = new("contact",
                      familyname = testNull(y$familyname, NA_character_),
                      givennames = testNull(y$givennames, NA_character_)),
                      order = y$order)
                }))

  return(result)
}
