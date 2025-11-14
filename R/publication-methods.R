#' @aliases names,publication-method
#' @rdname names
setMethod(f = "names",
          signature = signature(x = "publication"),
          definition = function(x) {
            slotNames("publication")
          })

#' @aliases names,publications-method
#' @rdname names
setMethod(f = "names",
          signature = signature(x = "publications"),
          definition = function(x) {
            slotNames("publication")
          })

#' @aliases show,publications-method
#' @rdname show
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

#' @aliases cash,publication-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "publication"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @aliases sub-sub,publications-method
#' @rdname sub-sub
setMethod(f = "[[",
          signature = signature(x = "publications", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("publication", x@publications[[i]])
            } else {
              out <- map(i, function(z) {
                new("publication", x@publications[[z]])
              })
              out <- new("publications", publications = out)
            }
            return(out)
          })

#' @aliases sub-subset,publications-method
#' @rdname sub-subset
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

#' @aliases length,publications-method
#' @rdname length
setMethod(f = "length",
          signature = signature(x = "publications"),
          definition = function(x) {
            length(x@publications)
          })

#' @aliases c,publications-method
#' @rdname c
setMethod(f = "c",
          signature = signature(x = "publications"),
          definition = function(x, y) {
            new("publications",
                publications = unlist(c(x@publications,
                                        y@publications),
                                      recursive = FALSE))
          })

#' @aliases show,publication-method
#' @rdname show
setMethod(f = "show",
          signature = signature(object = "publication"),
          definition = function(object) {
            print(data.frame(publicationid = object@publicationid,
                             citation = object@citation,
                             doi = object@doi))
          })

#' @title Show matched publication objects.
#' @param x A \code{publication} object.
#' @returns NULL
#' @noRd
#' @keywords internal
setMethod(f = "showMatch",
          signature = signature(x = "publication"),
          definition = function(x) {
            if (!is.null(attr(x, "matches"))) {
              print(attr(x, "matches"))
            }
          })

#' @aliases doi,publication-method
#' @rdname doi
setMethod(f = "doi",
          signature = signature(x = "publication"),
          definition = function(x) {
            x@doi
          })

#' @aliases as.data.frame,publication-method
#' @rdname as.data.frame
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

#' @aliases as.data.frame,publication-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature(x = "publication"),
          definition = function(x) {
            slots <- slotNames(x)
            slots <- slots[!slots == "author"]
            table <- slots %>%
              map(function(s) {
                out <- data.frame(slot(x, s),
                                  stringsAsFactors = FALSE)
                colnames(out) <- s
                return(out)
              }) %>%
              bind_cols()
            table$authors <- as.data.frame(x@author)
            return(table)
          })

#' @aliases as.data.frame,publication-method
#' @rdname as.data.frame
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
#' @noRd
#' @keywords internal
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

#' @noRd
#' @keywords internal
setMethod(f = "selectMatch",
          signature = signature(x = "publication", n = "logical"),
          definition = function(x, n) {
            attr(x, "matches") <- NULL
            return(x)
          })
