#' @title Reformat API author list to Neotoma contacts.
#' @author Simon Goring \email{goring@wisc.edu}
#' @param x A list coming from the `author` element of
#'  the Neotoma publicaiton API
#' @importFrom purrr map
#' @importFrom methods new
#' @returns `author` object
#' @noRd
pubAuthors <- function(x) {
  x <- cleanNULL(x)
  result <- new("authors",
                authors = map(x$author, function(y) {
                  new("author",
                      author = new("contact",
                      familyname = use_na(y$familyname, "char"),
                      givennames = use_na(y$givennames, "char")),
                      order = y$order)
                }))
  return(result)
}
