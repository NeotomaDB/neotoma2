#' @title Get publication information for Neotoma records
#' @description Uses the Neotoma API to search and access information
#' about publications associated with data in the Neotoma Paleoecology Database
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
#' @importFrom purrr pluck
#' @returns `publications` object
#' @examples \donttest{
#' # How old are the papers in Neotoma that include the term "mammut"?
#' mammoth_papers <- get_publications(search="mammut") %>%
#'   as.data.frame()
#' hist(as.numeric(mammoth_papers$year))
#' }
#' @export
get_publications <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_publications", x)
  } else {
    UseMethod("get_publications", NA)
  }
}
#' @title Get publication information from Neotoma
#' @importFrom methods new
#' @importFrom purrr pluck
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
#' @examples \donttest{
#' # How old are the papers in Neotoma that include the term "mammut"?
#' mammoth_papers <- get_publications(search="mammut") %>%
#'   as.data.frame()
#' hist(as.numeric(mammoth_papers$year))
#' }
#' @returns `publications` object
#' @export
get_publications.default <- function(...) {
  . <- ""
  baseURL <- paste0("data/publications") # nolint
  result <- parseURL(baseURL, ...) %>%
    cleanNULL() %>%
    pluck("data") %>%
    pluck("result")
  testNull <- function(val, out) { # nolint
    if (is.null(val)) {
      return(out)
    } else {
      return(val)
      }
  }
  pubs <- map(result, function(x) {
    if ("match" %in% names(x)) {
      match <- x$match
    } else {
      match <- NULL
    }
    x <- x$publication
    x[is.null(x)] <- NA_character_
    output <- new("publication",
        publicationtype = as.character(x$pubtype),
        publicationid = as.integer(x$publicationid),
        articletitle = as.character(x$articletitle),
        year = as.character(x$year),
        journal = as.character(x$journal),
        volume = as.character(x$volume),
        issue = as.character(x$issue),
        pages = as.character(x$pages),
        citation = as.character(x$citation),
        doi = as.character(x$doi),
        author = pubAuthors(x))
    attr(output, "match") <- match
    return(output)
  }) %>%
    new("publications", publications = .)
  return(pubs)
}
#' @title Get publications using their unique identifier.
#' @importFrom methods new
#' @importFrom purrr pluck
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
#' @examples {
#' # We want the paper identified in Neotoma as 666:
#' get_publications(666)
#' }
#' @returns `publications` object
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
  result <- parseURL(baseURL) %>% cleanNULL() # nolint
  pubs <- map(result$data, function(x) {
                  x <- x$publication
                  x[is.null(x)] <- NA_character_
                  new("publication",
                      publicationtype = as.character(x$pubtype),
                      publicationid = as.integer(x$publicationid),
                      articletitle = as.character(x$articletitle),
                      year = as.character(x$year),
                      journal = as.character(x$journal),
                      volume = as.character(x$volume),
                      issue = as.character(x$issue),
                      pages = as.character(x$pages),
                      citation = as.character(x$citation),
                      doi = as.character(x$doi),
                      author = pubAuthors(x))
                }) %>%
    new("publications", publications = .)
  return(pubs)
}

#' @title Update information for a publications object.
#' @description This works for records without publicationids. We assume that
#' data with publicationids is correct.
#' @importFrom dplyr coalesce
#' @param x integer A publication
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
#' @examples \donttest{
#' # Take a publication object and purposely degrade the metadata:
#' bad_pub <- get_publications(666)
#' # Note this only changes the reported year, not the citation string.
#' bad_pub[[1]]@year <- "1923"
#' bad_pub[[1]]@publicationid <- as.character(NA)
#' updated_pubs <- get_publications(bad_pub[[1]])
#' attr(updated_pubs, "matches")
#' # we see the proper citation in the record:
#' updated_pubs <- attr(updated_pubs, "matches")[[3]]
#' }
#' @returns updated `publication` object
#' @export
get_publications.publication <- function(x, ...) {
  if (is.na(x@publicationid)) {
    if (!is.na(x@citation)) {
      test <- get_publications(search = x@citation, limit = 3)
      attr(x, "matches") <- test
    } else {
      searchString <- dplyr::coalesce(x@citation, x@articletitle, x@booktitle) # nolint
      test <- get_publications(search = searchString, limit = 3)
      attr(x, "matches") <- test
    }
  }
  return(x)
}

#' @title Update metadata for a set of publication objects.
#' @importFrom dplyr coalesce
#' @param x integer A publication
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
#' @examples \donttest{
#' # Take a publication object and purposely degrade the metadata:
#' bad_pub <- get_publications(c(666, 667, 668))
#' # Note this only changes the reported year, not the citation string.
#' bad_pub[[1]]@year <- "1923"
#' bad_pub[[1]]@publicationid <- as.character(NA)
#' updated_pubs <- get_publications(bad_pub)
#' # Only the first publication object has any matches. It's the only one
#' # that is missing its publicaitonid.
#' attr(updated_pubs[[1]], "matches")
#' attr(updated_pubs[[2]], "matches")
#' # we see the proper citation in the record:
#' updated_pubs[[1]] <- attr(updated_pubs[[1]], "matches")[[1]]
#' }
#' @returns `publications` object
#' @export
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