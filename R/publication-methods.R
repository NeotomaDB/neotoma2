#' @title Get slot names for a publication object.
#' @param x A \code{publication} object.
#' @importFrom methods slotNames
#' @returns `string` with `publication` slots' names
#' @export
setMethod(f = "names",
          signature = signature(x = "publication"),
          definition = function(x) {
            slotNames("publication")
          })

#' @title Get slot names for a publication object.
#' @param x A \code{publications} object.
#' @importFrom methods slotNames
#' @returns `string` with `publications` slots' names
#' @export
setMethod(f = "names",
          signature = signature(x = "publications"),
          definition = function(x) {
            slotNames("publication")
          })

#' @title Show the contents of a publication object.
#' @param object A \code{publications} object
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @returns NULL - side effect function of printing a data.frame
#' @export
setMethod(f = "show",
          signature = signature(object = "publications"),
          definition = function(object) {
            map(object@publications, function(x) {
              data.frame(publicationid = x@publicationid,
                         citation = x@citation,
                         doi = x@doi)
            }) %>%
              bind_rows() %>%
              print()
          })

#' @title Extract an element from a \code{publication}
#' @param x A \code{publication} object.
#' @param name The slot to obtain (e.g., \code{articletitle})
#' @importFrom methods slot
#' @returns `value` in the selected slot
#' @export
setMethod(f = "$",
          signature = signature(x = "publication"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title Obtain one of the elements within a publication list.
#' @param x A publications object.
#' @param i A numeric index for the requested publication
#' @returns selected `publications` object from index
#' @export
setMethod(f = "[[",
          signature = signature(x = "publications", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("publication", x@publications[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("publication", x@publications[[z]])
                })
              out <- new("publications", publications = out)
            }
            return(out)
          })

#' @title Assign value to an element in a publication list.
#' @param x A publications object.
#' @param i A numeric index for the requested publication
#' @param value The value to be used
#' @returns `publications` with new assigned value.
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "publications"),
          definition = function(x, i, value) {
            if (length(i) == 1) {
              x@publications[[i]] <- new("publication", value)
              out <- x
              return(out)
            } else {
              warning("You can only reassign one publication at a time.")
            }
          })

#' @title Get the number of publications in a publications object.
#' @param x A publications object.
#' @returns `int` of the length of the `publications` object
#' @export
setMethod(f = "length",
          signature = signature(x = "publications"),
          definition = function(x) {
            length(x@publications)
          })

#' @title Combine publication objects.
#' @param x A publications object.
#' @param y A publications object
#' @returns concatenated `publications` object
#' @export
setMethod(f = "c",
          signature = signature(x = "publications"),
          definition = function(x, y) {
            new("publications",
                publications = unlist(c(x@publications,
                                       y@publications), recursive = FALSE))
          })

#' @title Print publications to screen.
#' @param object A \code{publication} object.
#' @returns NULL - side effect function of printing a data.frame
#' @export
setMethod(f = "show",
          signature = signature(object = "publication"),
          definition = function(object) {
            print(data.frame(publicationid = object@publicationid,
                             citation = object@citation,
                             doi = object@doi))
          })

#' @title Show matched publication objects.
#' @param x A \code{publication} object.
#' @returns NULL printed matches with other publications
#' @export
setMethod(f = "showMatch",
          signature = signature(x = "publication"),
          definition = function(x) {
            if (!is.null(attr(x, "matches"))) {
              print(attr(x, "matches"))
            }
          })

#' @title Get a publication DOI.
#' @param x A \code{publication} object.
#' @importFrom methods slotNames
#' @returns `DOI` from a publication
#' @export
setMethod(f = "doi",
          signature = signature(x = "publication"),
          definition = function(x) {
            x@doi
          })

#' @title Convert a publication author to a \code{data.frame}
#' @param x An author
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @returns `data.frame` with publications metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature(x = "authors"),
          definition = function(x) {
            authors <- x@authors %>%
              map(function(y) {
                paste0(y@author@familyname, ", ",
                       y@author@givennames)
                }) %>%
              paste0(collapse = "; ")
            return(authors)
          })

#' @title Convert a \code{publication} to a \code{data.frame}
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @param x A \code{publication} object.
#' @returns `data.frame` with publications' metadata.
#' @export
setMethod(f = "as.data.frame",
          signature = signature(x = "publication"),
          definition = function(x) {
            slots <- slotNames(x)
            slots <- slots[!slots == "author"]
            table <- slots %>%
              purrr::map(function(s) {
                out <- data.frame(slot(x, s),
                                  stringsAsFactors = FALSE)
                colnames(out) <- s
                return(out)
              }) %>%
              bind_cols()
            table$authors <- as.data.frame(x@author)
            return(table)
          })

#' @title Convert publications to a \code{data.frame}
#' @param x A \code{publications} object.
#' @returns `data.frame` with publications' metadata.
#' @export
setMethod(f = "as.data.frame",
          signature = signature(x = "publications"),
          definition = function(x) {
            full <- x@publications %>%
            map(function(y) as.data.frame(y)) %>%
              bind_rows()
            return(full)
})

#' @title Select the best match (between a local record and a Neotoma match)
#' @param x A \code{publication} object
#' @param n The match number.
#' @returns the best match to the selected publication.
#' @export
setMethod(f = "selectMatch",
          signature = signature(x = "publication", n = "numeric"),
          definition = function(x, n) {
            if (is.null(attr(x, "matches"))) {
              stop("There are no existing matches.")
            } else if (n > length(attr(x, "matches"))) {
              stop("The requested match is not in the current list.")
            } else if (n <= length(attr(x, "matches"))) {
              return(attr(x, "matches")[[n]])
            }
          })

#' @title Select the best match (between a local record and a Neotoma match)
#' @param x A \code{publication} object
#' @param n The match number (in the case an NA is returned).
#' @returns the best match to the selected publication.
#' @export
setMethod(f = "selectMatch",
          signature = signature(x = "publication", n = "logical"),
          definition = function(x, n) {
            attr(x, "matches") <- NULL
            return(x)
          })
