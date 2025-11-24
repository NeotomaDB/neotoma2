#' @title Get publication information for Neotoma records
#' @name get_publications
#' @description Uses the Neotoma API to search and access information
#' about publications associated with data in the Neotoma Paleoecology Database
#' @importFrom methods new
#' @importFrom purrr pluck
#' @importFrom dplyr coalesce
#' @param x integer A contact ID
#' @param ...
#' `publicationid`
#'    The unique numeric identifier associated with a publication in Neotoma.
#' `datasetid`
#'    A unique identifier for a Neotoma dataset that is associated
#'    with a publication.
#' `familyname` The full or partial last name for an individual author.
#' `pubtype` The publication type, from `get_tables("publicationtypes")`.
#' `year` The year the publication was released.
#' `search` A plain text search string used to search the citation.
#' @returns `publications` object
#' @examples \donttest{
#' # How old are the papers in Neotoma that include the term "mammut"?
#' tryCatch({
#'   mammoth_papers <- get_publications(search="mammut") %>%
#'     as.data.frame()
#'   hist(as.numeric(mammoth_papers$year))
#' }, error = function(e) {
#'  message("Neotoma server not responding. Try again later.")
#' })
#' # We want the paper identified in Neotoma as 666:
#' tryCatch({
#' get_publications(666)
#' }, error = function(e) {
#' message("Neotoma server not responding. Try again later.")
#' })
#' # Take a publication object and purposely degrade the metadata:
#' tryCatch({
#'   bad_pub <- get_publications(666)
#'   # Note this only changes the reported year, not the citation string.
#'   bad_pub[[1]]@year <- "1923"
#'   bad_pub[[1]]@publicationid <- NA_integer_
#'   updated_pubs <- get_publications(bad_pub[[1]])
#'   attr(updated_pubs, "matches")
#'   # we see the proper citation in the record:
#'   updated_pubs <- attr(updated_pubs, "matches")[[3]]
#' }, error = function(e) {
#'  message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @md
#' @export
get_publications <- function(x = NA, ...) {
  if (missing(x)) {
    UseMethod("get_publications", "default")
  } else {
    UseMethod("get_publications", x)
  }
}

#' @rdname get_publications
#' @export
get_publications.default <- function(...) {
  . <- ""
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  get_params("publications")
  baseURL <- paste0("data/publications")
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  if (is.null(result$data)) {
    return(NULL)
  } else {
    result <- result$data %>% cleanNULL() %>% pluck("result")
    pubs <- map(result, function(x) {
      if ("match" %in% names(x)) {
        match <- x$match
      } else {
        match <- NULL
      }
      x <- x$publication
      x[is.null(x)] <- NA_character_
      output <- new("publication",
                    publicationtype = use_na(testNull(x$pubtype), "char"),
                    publicationid = use_na(testNull(x$publicationid), "int"),
                    articletitle = use_na(testNull(x$articletitle), "char"),
                    year = use_na(testNull(x$year), "char"),
                    journal = use_na(testNull(x$journal), "char"),
                    volume = use_na(testNull(x$volume), "char"),
                    issue = use_na(testNull(x$issue), "char"),
                    pages = use_na(testNull(x$pages), "char"),
                    citation = use_na(testNull(x$citation), "char"),
                    doi = use_na(testNull(x$doi), "char"),
                    author = pubAuthors(x))
      attr(output, "match") <- match
      return(output)
    }) %>%
      new("publications", publications = .)
    return(pubs)
  }
}

#' @rdname get_publications
#' @export
get_publications.numeric <- function(x, ...) {
  . <- ""
  if (length(x) > 0) {
    pubids <- paste0(x, collapse = ",")
  }
  testNull <- function(val, out) { # nolint
    if (is.null(val)) {
      return(out)
    } else {
      return(val)
    }
  }
  baseURL <- paste0("data/publications/", pubids) # nolint
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  if (is.null(result$data)) {
    return(NULL)
  } else {
    result <- result %>% cleanNULL() # nolint
    pubs <- map(result$data, function(x) {
      x <- x$publication
      x[is.null(x)] <- NA_character_
      new("publication",
          publicationtype = use_na(testNull(x$pubtype), "char"),
          publicationid = use_na(testNull(x$publicationid), "int"),
          articletitle = use_na(testNull(x$articletitle), "char"),
          year = use_na(testNull(x$year), "char"),
          journal = use_na(testNull(x$journal), "char"),
          volume = use_na(testNull(x$volume), "char"),
          issue = use_na(testNull(x$issue), "char"),
          pages = use_na(testNull(x$pages), "char"),
          citation = use_na(testNull(x$citation), "char"),
          doi = use_na(testNull(x$doi), "char"),
          author = pubAuthors(x))
    }) %>%
      new("publications", publications = .)
    return(pubs)
  }
}

#' @rdname get_publications
#' @export
get_publications.publication <- function(x, ...) {
  if (is.na(x@publicationid)) {
    if (!is.na(x@citation)) {
      test <- get_publications(search = x@citation, limit = 3)
      attr(x, "matches") <- test
    } else {
      searchString <- coalesce(x@citation, x@articletitle, x@booktitle) # nolint
      test <- get_publications(search = searchString, limit = 3)
      attr(x, "matches") <- test
    }
  }
  return(x)
}

#' @rdname get_publications
#' @export
#' @method get_publications publications
get_publications.publications <- function(x, ...) {
  for (i in seq_len(length(x))) {
    pub <- x[[i]]
    if (is.na(x[[i]]@publicationid)) {
      if (!is.na(pub@citation)) {
        test <- get_publications(search = pub@citation, limit = 3, ...)
        attr(pub, "matches") <- test
        x@publications[[i]] <- pub
      }
    }
  }
  return(x)
}

#' @rdname get_publications
#' @export
get_publications.sites <- function(x, ...) {
  ds_ids <- getids(x)$datasetid %>%
    unique() %>%
    unlist() %>%
    as.numeric()
  l <- nrow(ds_ids)
  ds_ids <- paste0(ds_ids, collapse = ",")
  output <- get_publications(datasetid = ds_ids, limit=l)
  return(output)
}