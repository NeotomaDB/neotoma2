#' @title get_taxa
#' @name get_taxa
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description Find the Neotoma datasets that contain a taxon, and return
#' them as a `sites` object.
#' @param x A taxon ID (integer, or a vector of IDs) in Neotoma.
#' @param ... accepted arguments, see details for more information.
#' @details
#' `get_taxa()` works in two steps. First it resolves the taxon you supplied
#' to taxon names, with [get_taxon()]. Then it asks Neotoma for the datasets
#' that contain those names, with [get_datasets()]. The second step is the
#' expensive one: the API can take a minute or more to answer a broad
#' `taxa=` filter, so a taxon ID, or a name with a wildcard, that stands for
#' many names produces a slow request. `get_taxa()` warns when it is about to
#' send more than 25 distinct names, and it never sends the dataset request
#' without a taxon filter: if no taxon is found it returns `NULL`.
#'
#' The taxon can be given in any one of the following ways:
#'  * `x` A taxon ID (integer) in Neotoma. Can be passed as a vector of IDs.
#'  * `taxonname` A taxon name, or a partial name using `%` as a wildcard,
#' which is resolved with [get_taxon()].
#'  * `taxa` One or more exact taxon names, used as they are.
#'
#' Any other argument that [get_datasets()] accepts (`datasettype`, `loc`,
#' `ageyoung`, `ageold`, `limit`, `offset`, `all_data`, ...) is passed on to
#' that second step.
#' @returns A Neotoma2 `sites` object holding the datasets with the requested
#' taxa, or `NULL` if no taxon or no dataset was found.
#' @examples \dontrun{
#' # Datasets containing taxon 1 (Abies), at most five of them:
#' abies <- get_taxa(1, limit = 5)
#' # The same, starting from the taxon name:
#' abies <- get_taxa(taxonname = "Abies", limit = 5)
#' }
#' @md
#' @export
get_taxa <- function(x = NA, ...) {
  if (missing(x)) {
    UseMethod("get_taxa", "default")
  } else {
    UseMethod("get_taxa", x)
  }
}

# The largest number of taxon names sent in one `taxa=` filter before
# `get_taxa()` warns that the request will be slow.
taxa_name_warn_limit <- 25

#' @title taxon_names
#' @description An internal helper that returns the distinct names in a `taxa`
#' object, or `character(0)` for `NULL` (nothing was found).
#' @param taxa_data A `taxa` object, or `NULL`.
#' @returns `character` vector of taxon names.
#' @noRd
taxon_names <- function(taxa_data) {
  if (is.null(taxa_data)) {
    return(character(0))
  }
  nm <- unique(as.data.frame(taxa_data)$taxonname)
  nm[!is.na(nm)]
}

#' @title datasets_with_taxa
#' @description An internal helper shared by the `get_taxa()` methods. It sends
#' one `get_datasets()` request filtered by taxon name, and refuses to send it
#' with no names: an empty filter would ask the API for every dataset.
#' @param taxon `character` taxon names.
#' @param dots A named list of the arguments to pass on to `get_datasets()`.
#' @returns A `sites` object, or `NULL` if there is nothing to look for.
#' @noRd
datasets_with_taxa <- function(taxon, dots) {
  taxon <- unique(taxon)
  if (length(taxon) == 0) {
    message("No taxon found with the provided information. ",
            "No datasets were requested.")
    return(NULL)
  }
  if (length(taxon) > taxa_name_warn_limit) {
    warning(length(taxon), " taxon names will be sent to the API in one ",
            "request. Broad `taxa` queries can take a minute or more to ",
            "answer. Consider a narrower taxon name or ID, or add other ",
            "filters such as `datasettype`, `loc`, or `limit`.",
            call. = FALSE)
  }
  do.call(get_datasets, c(list(taxa = taxon), dots))
}

#' @rdname get_taxa
#' @method get_taxa numeric
#' @exportS3Method get_taxa numeric
get_taxa.numeric <- function(x, ...) {
  oo <- options(scipen = 9999)
  on.exit(options(oo))
  # Only the taxon ID goes to the taxa endpoint: the dataset arguments in `...`
  # (limit, offset, all_data, loc, ...) belong to the datasets request.
  datasets_with_taxa(taxon_names(get_taxon(x)), list(...))
}

#' @rdname get_taxa
#' @method get_taxa default
#' @exportS3Method get_taxa default
get_taxa.default <- function(...) {
  oo <- options(scipen = 9999)
  on.exit(options(oo))
  dots <- list(...)
  if ("taxa" %in% names(dots)) {
    taxon <- as.character(unlist(dots$taxa))
  } else if ("taxonname" %in% names(dots)) {
    taxon <- taxon_names(get_taxon(taxonname = dots$taxonname))
  } else {
    stop("Please provide a taxon ID, a taxon name (`taxonname`), ",
         "or exact taxon names (`taxa`) to extract site information.")
  }
  dots$taxa <- NULL
  dots$taxonname <- NULL
  datasets_with_taxa(taxon, dots)
}
