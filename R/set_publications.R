#' @title Create a new publication (or publication set)
#' @description A function to create new publication objects by hand.
#' @param publicationtype A text string identifying the publication type within the Neotoma database.
#' @param year The year of publication.
#' @param citation A full text citation for the article.
#' @param articletitle The title of the article.
#' @param journal The journal in which the article was published.
#' @param volume The journal volume.
#' @param issue The journal issue.
#' @param pages The pages of the journal.
#' @param citationnumber How many times has the paper been cited?
#' @param doi A DOI for the record.
#' @param booktitle The title of the book (if the publication is a book)
#' @param numvolumes The number of book volumes (if a series)
#' @param edition The book edition.
#' @param volumetitle The title of the volume (in a published series)
#' @param seriestitle The title of the series.
#' @param seriesvolume The series volume.
#' @param publisher The publisher.
#' @param url Publication URL
#' @param city City of publication.
#' @param state State of publication.
#' @param country Country of publication.
#' @param originallanguage Original language of publication.
#' @param notes Publication notes.

set_publication <- function(...) {
    new("publication",
        publicationtype = as.character(pubtype),
        publicationid = as.numeric(publicationid),
        articletitle = as.character(articletitle),
        year = as.character(year),
        journal = as.character(journal),
        volume = as.character(volume),
        issue = as.character(issue),
        pages = as.character(pages),
        citation = as.character(citation),
        doi = as.character(doi),
        author = author)
}