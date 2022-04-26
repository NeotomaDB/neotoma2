#' @title Extract datasets from a sites object.
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @importFrom purrr map reduce
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "collunits",
          signature = "sites",
          definition = function(object) {
            output <- purrr::map(object@sites, function(x) {
              x@collunits
            })
            
            final <- purrr::reduce(output, c)
            
            return(final)
          })

#' @title Extract datasets from a sites object.
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @export
setMethod(f = "collunits",
          signature = "site",
          definition = function(object) {
            output <- object@collunits
            attr(output, "siteid") <- object$siteid
            return(output)
          })


#' @title Extract datasets from a collunits object.
#' @param object A collunits object
#' @importFrom purrr map reduce
#' @export
setMethod(f = "datasets",
          signature = "collunits",
          definition = function(object) {
            result <- purrr::map(object@collunits, function(x)x@datasets)
            if (length(result) == 1) {
              out <- result[[1]]
            } else {
              out <- purrr::reduce(result, c)
            }
            return(out)
          })

#' @title Extract datasets from a collunit object.
#' @param object A collunits object
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @export
setMethod(f = "datasets",
          signature = "collunit",
          definition = function(object) {
            result <- object@datasets
            return(result)
          })

#' @title Extract datasets from a sites object.
#' @param object A sites object
#' @importFrom purrr map
#' @export
setMethod(f = "datasets",
          signature = "sites",
          definition = function(object) {
            datasets(collunits(object))
          })

#' @title Extract datasets from a sites object.
#' @param object A sites object
#' @importFrom methods slotNames slot
#' @importFrom purrr map
#' @export
setMethod(f = "datasets",
          signature = "site",
          definition = function(object) {
            cunits <- collunits(object)
            result <- purrr::map(cunits@collunits, function(x)x@datasets)
            if (length(result) == 1) {
              out <- result[[1]]
            } else {
              out <- purrr::reduce(result, c)
            }
            return(out)
          })

#' @title Extract chronologies from a sites object.
#' @param x A collunit object
#' @importFrom methods slotNames slot
#' @export
setMethod(f = "chronologies",
          signature = "collunit",
          definition = function(x) {
            output <- map(x@chronologies@chronologies, function(y) {
              attr(y, "collunitid") <- x$collectionunitid
              return(y)
            })
            try(
              new("chronologies", chronologies = output)
            )
            if ('try-error' %in% class(new("chronologies", chronologies = output))) {
              stop("not today")
              
            } else {
              new("chronologies", chronologies = output)
            }
          })

#' @title Extract chronologies from a sites object.
#' @param x A collunit object
#' @importFrom methods slotNames slot
#' @export
setMethod(f = "chronologies",
          signature = "collunits",
          definition = function(x) {
            output <- map(x@collunits, function(y) {
              chronologies(y)
            })
            
            output <- purrr::reduce(output, c)
            return(output)
          })

#' @title Extract chronologies from a sites object.
#' @param x A site object
#' @importFrom methods slotNames slot
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

#' @title Extract chronologies from a sites object.
#' @param x A sites object
#' @importFrom methods slotNames slot
#' @export
setMethod(f = "chronologies",
          signature = "sites",
          definition = function(x) {
            output <- map(x@sites, function(y) {
              chronologies(y)
            })
            output <- purrr::reduce(output, c)
            return(output)
          })
