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
#' @examples 
#' onePub <- get_publications(12)
#' @export

get_publications <- function(x = NA, ...) {
  if(!missing(x)) {
    UseMethod('get_publications', x)
  } else {
    UseMethod('get_publications', NA)
  }
}

#' @title Get contact information for Neotoma contributors
#' @importFrom methods new
#' @importFrom purrr pluck
#' @export
get_publications.default <- function(...) {
  
  baseURL <- paste0('data/publications')
  result <- parseURL(baseURL, ...) %>% 
    cleanNULL() %>% 
    pluck("data") %>% 
    pluck("result")
  
  pubs <- map(result, function(x) {
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
        author = newAuthors(x)) 
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

  newAuthors <- function(x) {
    if(is.null(x$author)) {
      result <- new("authors",
                          authors = list(
                            new("author",
                                author = new("contact",
                                            familyname = NA_character_,
                                            givennames= NA_character_,
                                            order = 1)
                                      )))
    }

    result <- new("authors",
                        authors = map(x$author, function(y) {
                          new("author",
                              author = new("contact",
                                           familyname =as.character(y$family),
                                           givennames= as.character(y$given)),
                                           order = as.numeric(y$order))
                                     }))
    return(result)
  }
  
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
                      author = newAuthors(x)) 
                }) %>% 
    new("publications", publications = .)
  return(pubs)
}

#' @title newAuthors
#' @description This function is a helper, not exported.
newAuthors <- function(x) {
  if(is.null(x$author)) {
    result <- new("authors",
                  authors = list(
                    new("author",
                        author = new("contact",
                                     familyname = NA_character_,
                                     givennames= NA_character_,
                                     order = 1)
                    )))
  }
  
  result <- new("authors",
                authors = map(x$author, function(y) {
                  new("author",
                      author = new("contact",
                                   familyname =as.character(y$family),
                                   givennames= as.character(y$given)),
                      order = as.numeric(y$order))
                }))
  return(result)
}
