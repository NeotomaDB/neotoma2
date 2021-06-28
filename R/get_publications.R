#' @title Get publication information for Neotoma records
#' @description Uses the Neotoma API to search and access information about publications associated with data in the Neotoma Paleoecology Database
#' @param x integer A contact ID
#' @param publicationid The unique numeric identifier associated with a publication in Neotoma.
#' @param datasetid A unique identifier for a Neotoma dataset that is associated with a publication.
#' @param familyname The full or partial last name for an individual author.
#' @param pubtype The publication type, from `get_tables('publicationtypes')`.
#' @param year The year the publication was released.
#' @param search A plain text search string used to search the citation.
#' @importFrom purrr pluck
#' @export

get_publications <- function(x = NA, ...) {
  if(!missing(x)) {
    UseMethod('get_publications', x)
  } else {
    UseMethod('get_publications', NA)
  }
}

#' @title Get publication information from Neotoma  
#' @importFrom methods new
#' @importFrom purrr pluck
#' @export
get_publications.default <- function(...) {
  
  baseURL <- paste0('data/publications')
  result <- parseURL(baseURL, ...) %>% 
    cleanNULL() %>% 
    pluck("data") %>% 
    pluck("result")
  
  testNull <- function(val, out) {
    if(is.null(val)) { return(out)} else {return(val)}
  }
    
  pubs <- map(result, function(x) {
    
    if ('match' %in% names(x)) {
      match <- x$match
    } else {
      match <- NULL
    }
    
    x <- x$publication
    x[is.null(x)] <- NA_character_
    
    output <- new("publication",
        publicationtype = as.character(x$pubtype),
        publicationid = as.numeric(x$publicationid),
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
    return (output)
  }) %>% 
    new("publications", publications = .)
  
  return(pubs)
}

#' @title Get contact information for Neotoma contributors
#' @importFrom methods new
#' @importFrom purrr pluck
#' @export
get_publications.numeric <- function(x, ...) {

  if (length(x) > 0) {
    pubids <- paste0(x, collapse = ',')
  }

  testNull <- function(val, out) {
    if(is.null(val)) { return(out)} else {return(val)}
  }
  
  baseURL <- paste0('data/publications/', pubids)

  result <- parseURL(baseURL) %>% cleanNULL()

  pubs <- map(result$data, function(x) {
    
                  x <- x$publication
                  x[is.null(x)] <- NA_character_
                  
                  new("publication",
                      publicationtype = as.character(x$pubtype),
                      publicationid = as.numeric(x$publicationid),
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

#' @importFrom dplyr coalesce
#' @export
get_publications.publication <- function(x, ...) {
  if(is.na(x@publicationid)) {
    if(!is.na(x@citation)) {
      test <- get_publications(search = x@citation, limit = 3)
      attr(x, "matches") <- test
    } else {
      searchString <- dplyr::coalesce(x@citation, x@articletitle, x@booktitle)
      test <- get_publications(search = searchString, limit = 3)
      attr(x, "matches") <- test
    }
  }
  return(x)
}

#' @export
get_publications.publications <- function(x, ...) {
  if ('verbose' %in% names(...)) {
    
  }
  for (i in 1:length(x)) {
    pub <- x[[i]]
    if(is.na(x[[i]]@publicationid)) {
      if(!is.na(pub@citation)) {
        test <- get_publications(search = pub@citation, limit = 3, ...)
        attr(pub, "matches") <- test
        x@publications[[i]] <- pub
      }
    }
  }
  return(x)
}