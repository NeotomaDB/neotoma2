#' @title Get publication information for Neotoma records
#' @description Uses the Neotoma API to search and access information about publications associated with data in the Neotoma Paleoecology Database
#' @param x integer A contact ID
#' @param publicationid The unique numeric identifier associated with a publication in Neotoma.
#' @param datasetid A unique identifier for a Neotoma dataset that is associated with a publication.
#' @param familyname The full or partial last name for an individual author.
#' @param pubtype The publication type, from `get_tables('publicationtypes')`.
#' @param year The year the publication was released.
#' @param search A plain text search string used to search the citation.
#' @export

get_publications <- function(x = NA, ...) {
  UseMethod('get_publications')
}

#' @title Get contact information for Neotoma contributors
#' @importFrom methods new
#' @export
get_publications.default <- function(x, ...) {
  
  baseURL <- paste0('data/publications')
  result <- parseURL(baseURL, ...) %>% 
    cleanNULL() %>% 
    pluck("data") %>% 
    pluck("result")
  
  pubs <- map(result, function(x) {
    
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
        author = x$author) 
  }) %>% 
    new("publications", publications = .)
  
  return(pubs)
}

#' @title Get contact information for Neotoma contributors
#' @importFrom methods new
#' @export
get_publications.numeric <- function(x, ...) {

  if (length(x) > 0) {
    pubids <- paste0(x, collapse = ',')
  }

  baseURL <- paste0('data/publications/', pubids)

  result <- parseURL(baseURL) %>% cleanNULL()

  pubs <- map(result$data, function(x) {
    
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
                      author = x$author) 
                }) %>% 
    new("publications", publications = .)
  return(pubs)
}