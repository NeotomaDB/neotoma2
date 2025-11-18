#' @title Helper function for `get_speleothems`
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @param sites A `sites` object
#' @return Processed speleothem data in the `sites` object
#' @noRd
speleo_helper <- function(sites) {
  ids <- getids(sites)
  cuids <- ids$collunitid
  if (length(cuids) > 0) {
    cuids <- paste0(cuids, collapse = ",")
  }
  baseURL <- paste0("data/speleothems/", cuids)
  result <- tryCatch(
    parseURL(baseURL),
    error = function(e) {
      message("API call failed: ", e$message)
      NULL
    }
  )
  if (length(result$data) > 0) {
    speleo <- parse_speleothem(result$data)
    speleo <- speleo %>% cleanNULL()
  } else {
    speleo <- NULL
  }
  if (!is.null(speleo)) {
    pared_ds <- map(sites@sites, function(x) {
      ycu <- map(x@collunits, function(z) {
        yds <- speleo[
          which(as.data.frame(speleo)$collectionunitid %in% z$collectionunitid)
        ]
        z@speleothems <- yds
        z
      })
      x@collunits@collunits <- ycu
      x
    })
  } else {
    pared_ds <- NULL
  }
  pared_ds
}

#' @title get_speleothems
#' @name get_speleothems
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom methods new
#' @param x A dataset ID or vector of dataset IDs
#' @param ... accepted arguments
#' @returns `sites` object with speleothem data
#' @details
#' Experimental function: API and behavior may change.
#' The `get_speleothems()` command wraps the Neotoma API
#' ([api.neotomadb.org](https://api.neotomadb.org)) call for `speleothems`.
#' The call itself uses a SQL query which accepts any one of the following
#' parameters:
#'  * `x`  The unique dataset ID (integer) in Neotoma. Can be passed as a
#' vector of dataset IDs.
#'  * `sites`  A `sites` R object.
#' @examples {
#' ## Find speleothems by numeric datasetid:
#' tryCatch({
#'   speleo <- get_speleothems(c(2,5))
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @md
#' @export
get_speleothems <- function(x = NA, ...) {
  if (missing(x)) {
    UseMethod("get_speleothems", "default")
  } else {
    UseMethod("get_speleothems", x)
  }
}


#' @rdname get_speleothems
#' @method get_speleothems numeric
#' @exportS3Method get_speleothems numeric
get_speleothems.numeric <- function(x, ...) {
  st <- get_datasets(x)
  pared_ds <- speleo_helper(st)
  if (is.null(pared_ds)) {
    st
  } else {
    new("sites", sites = pared_ds)
  }
}

#' @rdname get_speleothems
#' @method get_speleothems sites
#' @exportS3Method get_speleothems sites
get_speleothems.sites <- function(x, ...) {
  pared_ds <- speleo_helper(x)
  if (is.null(pared_ds)) {
    x
  } else {
    new("sites", sites = pared_ds)
  }
}