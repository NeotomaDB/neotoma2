# Start "c" methods
#' @title c Method - Combine objects, including NULL
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @title c Method for NULL values
#' @param x NULL object
#' @param y sites/datasets object
#' @returns `list` of concatenated items when the first object is NULL
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x = "missingORNULL", y) {
            y
          })
