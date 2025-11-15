#' @title Extract `collunits` from a `sites` object.
#' @param object A `sites` object
#' @importFrom purrr map reduce
#' @returns `collunits` from a `sites` object
#' @md
#' @export
setMethod(f = "collunits",
          signature = "sites",
          definition = function(object) {
            output <- map(object@sites, function(x) {
              x@collunits
            })
            if (length(output) == 0) {
              return(new("collunits", collunits = list()))
            }
            final <- reduce(output, c)
            final <- clean(final)
            return(final)
          })

#' @rdname collunits
#' @export
setMethod(f = "collunits",
          signature = "site",
          definition = function(object) {
            output <- object@collunits
            attr(output, "siteid") <- object$siteid
            return(output)
          })

#' @title Extract `datasets` from a `sites` or `collunits` object.
#' @importFrom purrr map reduce
#' @param object A `sites` or`collunits` object
#' @returns `datasets` object
#' @export
setMethod(f = "datasets",
          signature = "collunits",
          definition = function(object) {
            result <- map(object@collunits,
              function(x) x@datasets)
            if (length(result) == 1) {
              out <- result[[1]]
            } else {
              out <- reduce(result, c)
            }
            return(out)
          })

#' @rdname datasets
#' @export
setMethod(f = "datasets",
          signature = "collunit",
          definition = function(object) {
            result <- object@datasets
            return(result)
          })

#' @rdname datasets
#' @export
setMethod(f = "datasets",
          signature = "sites",
          definition = function(object) {
            clean(datasets(collunits(object)))
          })

#' @rdname datasets
#' @export
setMethod(f = "datasets",
          signature = "site",
          definition = function(object) {
            cunits <- collunits(object)
            result <- map(cunits@collunits,
              function(x) x@datasets)
            if (length(result) == 1) {
              out <- result[[1]]
            } else {
              out <- reduce(result, c)
            }
            return(out)
          })

#' @title Extract `chronologies` from a `site`s or `collunits` object.
#' @importFrom purrr map
#' @param x A `sites` or `collunits` object
#' @returns `chronologies` from a `collunit` object
#' @md
#' @export
setMethod(f = "chronologies",
          signature = "collunit",
          definition = function(x) {
            output <- map(x@chronologies@chronologies, function(y) {
              attr(y, "collunitid") <- x$collectionunitid
              return(y)
            })
            try(
              returner <- new("chronologies", chronologies = output)
            )
            if ("try-error" %in% class(returner)) {
              stop("Cannot create chronology for this colleciton unit.")
            }
            return(returner)
          })

#' @rdname chronologies
#' @export
setMethod(f = "chronologies",
          signature = "collunits",
          definition = function(x) {
            output <- map(x@collunits, function(y) {
              chronologies(y)
            })
            output <- reduce(output, c)
            return(output)
          })

#' @rdname chronologies
#' @export
setMethod(f = "chronologies",
          signature = "site",
          definition = function(x) {
            output <- map(x@collunits@collunits, function(y) {
              chronologies(y)
            })
            output <- purrr::reduce(output, c)
            return(output)
          })

#' @rdname chronologies
#' @export
setMethod(f = "chronologies",
          signature = "sites",
          definition = function(x) {
            output <- map(x@sites, function(y) {
              chronologies(y)
            })
            output <- reduce(output, c)
            return(output)
          })