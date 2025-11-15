#' @title Parse Taxon Results
#' @importFrom purrr map
#' @param result The result object from the Neotoma API call
#' @returns A `taxa` object with the parsed taxon information
#' @noRd
parse_taxon <- function(data) {
  data <- data$data %>%
    cleanNULL()
  newTaxa <- map(data, function(x) {
    new("taxon",
        taxonid = use_na(testNull(x$taxonid, NA), "int"),
        taxoncode = use_na(testNull(x$taxoncode, NA), "char"),
        taxonname = use_na(testNull(x$taxonname, NA), "char"),
        author = use_na(testNull(x$author, NA), "char"),
        ecolgroup = use_na(testNull(x$ecolgroup, NA), "char"),
        highertaxonid = use_na(testNull(x$highertaxonid, NA), "int"),
        status = use_na(testNull(x$status, NA), "char"),
        taxagroupid = use_na(testNull(x$taxagroupid, NA), "int"),
        publicationid = use_na(testNull(x$publicationid, NA), "int"),
        publication = use_na(testNull(x$publication, NA), "char"))
  })
  taxa <- new("taxa", taxa = newTaxa)
  return(taxa)
}

#' @title get_taxon
#' @name get_taxon
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description a `taxon` object with detailed information
#' @param x taxon ID
#' @param ... accepted arguments, see **details** for more information.
#' @returns A Neotoma2 `taxa` object with datasets with the requested taxa.
#' @details
#' A `taxa` may have one or more taxa associated with it.
#' The `get_taxon` function searches for `taxa` detail data within Neotoma
#' while the `get_taxa` function searches for sites that contain the requested
#' taxa. The function searches for each taxon by using a SQL query which
#' accepts any one of the following parameters:
#' * `taxonid`  The unique taxon ID (integer) in Neotoma. Can be passed as a
#' vector of taxa IDs.
#'  * `taxonname` Taxon name or partial name. You can pass wildcard characters
#' using %.
#'  * `taxagroup` The taxonomic grouping the taxon belongs to, from the Neotoma
#' taxagrouptypes table.
#'  * `ecolgroup` The ecological group of interest for the Neotoma taxon (from
#' the Neotoma ecolgrouptypes table)
#'  * `status` The taxonomic extinction status of the taxon, either extinct 
#' (1, True) or extant (0, False).
#' @md
#' @export
get_taxon <- function(x = NA, ...) {
  if (missing(x)) {
    UseMethod("get_taxon", "default")
  } else {
    UseMethod("get_taxon", x)
  }
}

#' @rdname get_taxon
#' @export
#' @method get_taxon default
get_taxon.default <- function(...) {
  oo <- options(scipen = 9999)
  on.exit(options(oo))
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  if ("all_data" %in% cl) {
    all_data <- cl$all_data
  } else {
    all_data <- TRUE
  }
  params <- get_params("taxa")
  if (!all(names(cl) %in% params)) {
    warning("Some parameters seem invalid. The current accepted parameters
             are: ", paste(unlist(params), collapse = ", "))
  }
  baseURL <- paste0("data/taxa")
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  if (is.null(result)) {
    return(NULL)
  } else {
    result <- result %>%
      cleanNULL()
    if (is.null(result$data[1][[1]]) || is.null(result[1][[1]])) {
      message("No taxon data found with the provided parameters.")
      return(NULL)
    } else {
      output <- parse_taxon(result)
      return(output)
    }
  }
}

#' @rdname get_taxon
#' @export
#' @method get_taxon numeric
get_taxon.numeric <- function(x, ...) {
  if (length(x) > 0) {
    taxa_id <- paste0(x, collapse = ",")
  }
  baseURL <- paste0("data/taxa/", taxa_id)
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  result_length <- length(result[2]$data)
  if (result_length > 0) {
    output <- parse_taxon(result)
    return(output)
  } else {
    message("No taxon data found for taxon ID: ", taxa_id)
    return(NULL)
  }
}