#' @title Parse Taxon Results
#' @importFrom purrr map
#' @param result The result object from the Neotoma API call
#' @returns A `taxa` object with the parsed taxon information
#' @noRd
parse_taxon <- function(data) {
  data <- data$data %>%
    cleanNULL()
  new_taxa <- map(data, function(x) {
    new("taxon",
      taxonid = use_na(testNull(x$taxonid, NA), "int"),
      taxoncode = use_na(testNull(x$taxoncode, NA), "char"),
      taxonname = use_na(testNull(x$taxonname, NA), "char"),
      author = use_na(testNull(x$author, NA), "char"),
      ecolgroup = use_na(testNull(x$ecolgroup, NA), "char"),
      highertaxonid = use_na(testNull(x$highertaxonid, NA), "int"),
      status = use_na(testNull(x$status, NA), "char"),
      taxagroupid = use_na(testNull(x$taxagroupid, NA), "char"),
      publicationid = use_na(testNull(x$publicationid, NA), "int"),
      publication = use_na(testNull(x$publication, NA), "char")
    )
  })
  taxa <- new("taxa", taxa = new_taxa)
  taxa
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
  if ("all_data" %in% names(cl)) {
    all_data <- cl$all_data
  } else {
    all_data <- TRUE
  }
  params <- get_params("taxa")
  if (!all(names(cl) %in% params)) {
    warning("Some parameters seem invalid. The current accepted parameters
             are: ", paste(unlist(params), collapse = ", "))
  }
  base_url <- paste0("data/taxa")
  # `all_data` is handed to parseURL() explicitly, and taken out of `...` so it
  # is not passed twice. Without this a wildcard such as `taxonname = "Pinus%"`
  # came back as the API's first page only, with no warning.
  dots <- list(...)
  dots$all_data <- NULL
  result <- tryCatch(
    do.call(parseURL, c(list(base_url, all_data = all_data), dots)),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  if (is.null(result)) {
    NULL
  } else {
    result <- result %>%
      cleanNULL()
    if (is.null(result$data[1][[1]]) || is.null(result[1][[1]])) {
      message("No taxon data found with the provided parameters.")
      NULL
    } else {
      output <- parse_taxon(result)
      output
    }
  }
}

#' @rdname get_taxon
#' @export
#' @method get_taxon numeric
get_taxon.numeric <- function(x, ...) {
  # An NA would become `data/taxa/NA`, which the API answers with a database
  # error rather than "nothing found".
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    message("No taxon ID provided.")
    return(NULL)
  }
  oo <- options(scipen = 9999)
  on.exit(options(oo))
  taxa_id <- paste0(unique(x), collapse = ",")
  # A long vector of ids makes a URL the API will not take. parseURL() then
  # sends the ids in several short GET requests (there is no POST route for
  # taxa), so it is safe to pass any number of ids here.
  base_url <- paste0("data/taxa/", taxa_id)
  result <- tryCatch(
    parseURL(base_url, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  result_length <- length(result$data)
  if (result_length > 0) {
    output <- parse_taxon(result)
    output
  } else {
    message(
      "No taxon data found for taxon ID: ",
      substr(taxa_id, 1, 60), if (nchar(taxa_id) > 60) "..."
    )
    NULL
  }
}
