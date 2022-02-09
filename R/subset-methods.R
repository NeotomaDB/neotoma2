#' @title Extract datasets from a collunits object.
#' @param object A collunits object
#' @importFrom purrr map
#' @export
setMethod(f = "datasets",
          signature = "collunits",
          definition = function(object) {
            result <- purrr::map(object@collunits, function(x)x@datasets)
            if (length(result) == 1) {
              out <- result[[1]]
            } else {
              out <- result[[1]]
              for (i in 2:length(result)) {
                out <- c(out, result[[i]])
              }
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
            my_datasets <- c()
            for (i in seq_len(length(object@sites))) {
              collunits_call <- object@sites[[i]]@collunits@collunits[[1]]
              my_dataset <- collunits_call@datasets@datasets[[1]]
              my_datasets <- append(my_datasets, my_dataset)
              my_datasets2 <- new("datasets", datasets = my_datasets)
            }
            return(my_datasets2)
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
              out <- result[[1]]
              for (i in 2:length(result)) {
                out <- c(out, result[[i]])
              }
            }
            return(out)
          })
