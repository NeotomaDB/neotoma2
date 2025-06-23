#' @md
#' @title get_taxon
#' @description a sites object with the requested taxa.
#' @param x string taxa name or names
#' @param ... accepted arguments, see details for more information.
#' @returns A Neotoma2 sites object with datasets with the requested taxa.
#' 
#' @export
get_taxon <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_taxon", x)
  } else {
    UseMethod("get_taxon", NA)
  }
}

parse_taxon <- function(result) { # nolint
  
  fix_null <- function(x) {
    for (i in seq_len(length(x))) {
      if (is.null(x[[i]])) {
        x[[i]] <- NA
      } else {
        if (is(x[[i]], "list")) {
          x[[i]] <- fix_null(x[[i]])
        }
      }
    }
    return(x)
  }
  
  data <- result$data %>%
    fix_null()
  
  newTaxa <- map(data, function(x) {
    new_taxon <- new("taxon",
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


#' @title Get Taxa Default
#' @param x Use a taxon ID to extract site information
#' @param ... accepted arguments, see details for more information.
#' @importFrom utils URLencode
#' @returns `sites` object containing the requested `taxa`
#' @export
get_taxon.default <- function(x, ...) {
  oo <- options(scipen = 9999999)
  on.exit(options(oo))
  cl <- as.list(match.call())
  
  cl[[1]] <- NULL
  
  cl <- lapply(cl, eval, envir = parent.frame())
  
  all_data <- ifelse(is.null(cl$all_data), FALSE, TRUE)
  error_check <- check_args(cl) # nolint
  
  if (error_check[[2]]$flag == 1) {
    stop(paste0(unlist(error_check[[2]]$message), collapse = "\n  "))
  } else {
    cl <- error_check[[1]]
  }
  
  base_url <- paste0("data/taxa")
  result <- parseURL(base_url, ...) %>%
    cleanNULL()
  
  if (is.null(result$data[1][[1]]) || is.null(result[1][[1]])) {
    return(NULL)
    
  } else {
    output <- parse_taxon(result)
    return(output)
  }
}

#' @title Get Taxa Numeric
#' @param x Use a taxon ID to extract sites information
#' @param ... Additional parameters to get_taxa
#' @returns `sites` object with requested `taxa`
#' @examples \donttest{
#' allds <- get_datasets(1:3)
#' }
#' @export
get_taxon.numeric <- function(x, ...) {
  use_na <- function(x, type) {
    if (is.na(x)) {
      return(switch(type,
                    "char" = NA_character_,
                    "int" = NA_integer_))
    } else {
      return(x)
    }
  }
  
  if (length(x) > 0) {
    taxa_id <- paste0(x, collapse = ",")
  }
  
  base_url <- paste0("data/taxa/", taxa_id)
  result <- neotoma2::parseURL(base_url, ...)
  result_length <- length(result[2]$data)
  
  if (result_length > 0) {
    output <- parse_taxon(result)
    return(output)
  } else {
    return(NULL)
  }
}