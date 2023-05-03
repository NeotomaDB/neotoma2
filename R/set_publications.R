#' @title Create a new publication (or publication set)
#' @description A function to create new publication objects by hand.
#' @param publicationtype A text string identifying the publication
#'  type within the Neotoma database.
#' @param publicationid ID of publication
#' @param publicationtypeid ID of kind of publication
#' @param author name of the author of publication.
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
#' @returns `publication` object
#' @export

set_publications <- function(
    publicationid = NA_integer_,
    publicationtypeid = NA_integer_,
    publicationtype = NA_character_,
    year = NA_character_,
    citation = NA_character_,
    articletitle = NA_character_,
    journal = NA_character_,
    volume = NA_character_,
    issue = NA_character_,
    pages = NA_character_,
    citationnumber = NA_character_,
    doi = NA_character_,
    booktitle = NA_character_,
    numvolumes = NA_character_,
    edition = NA_character_,
    volumetitle = NA_character_,
    seriestitle = NA_character_,
    seriesvolume = NA_character_,
    publisher = NA_character_,
    url = NA_character_,
    city = NA_character_,
    state = NA_character_,
    country = NA_character_,
    originallanguage = NA_character_,
    notes = NA_character_,
    author = NULL) {

    if (is.null(author)) {
        author <- new("authors")
    }

    if (is.na(citation)) {
        citation <- paste0(articletitle, journal)
    }

    new("publication",
        publicationid = as.numeric(publicationid),
        publicationtypeid = as.numeric(publicationtypeid),
        publicationtype = as.character(publicationtype),
        year = as.character(year),
        citation = as.character(citation),
        articletitle = as.character(articletitle),
        journal = as.character(journal),
        volume = as.character(volume),
        issue = as.character(issue),
        pages = as.character(pages),
        citationnumber = as.character(citationnumber),
        doi = as.character(doi),
        booktitle = as.character(booktitle),
        numvolumes = as.character(numvolumes),
        edition = as.character(edition),
        volumetitle = as.character(volumetitle),
        seriestitle = as.character(seriestitle),
        seriesvolume = as.character(seriesvolume),
        publisher = as.character(publisher),
        url = as.character(url),
        city = as.character(city),
        state = as.character(state),
        country = as.character(country),
        originallanguage = as.character(originallanguage),
        notes = as.character(notes),
        author = author)
}