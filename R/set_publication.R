#' @title Create a new publication (or publication set)
#' @author Simon Goring \email{goring@wisc.edu}
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @param x "publications" object to be updated.
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
#' @description A function to create new publication objects by hand.
#' @md
#' @export
set_publication <- function(
    x = NA,
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
  if (suppressWarnings(is.na(x))) {
    x <- new("publication")
    if (is.na(publicationid)) {
      hash <- digest(UUIDgenerate(), algo = "xxhash32", serialize = FALSE)
      x@publicationid <- as.integer(strtoi(substr(hash, 1, 7), base = 16L))
    } else {
      x@publicationid <- publicationid
    }
  }
  if (is.null(author)) {
    author <- new("authors", authors = list(new("author")))
  }
  if (is.na(citation)) {
    citation <- paste0(articletitle, journal)
  }
  x@publicationtypeid <- as.numeric(publicationtypeid)
  x@publicationtype <- as.character(publicationtype)
  x@year <- as.character(year)
  x@citation <- as.character(citation)
  x@articletitle <- as.character(articletitle)
  x@journal <- as.character(journal)
  x@volume <- as.character(volume)
  x@issue <- as.character(issue)
  x@pages <- as.character(pages)
  x@citationnumber <- as.character(citationnumber)
  x@doi <- as.character(doi)
  x@booktitle <- as.character(booktitle)
  x@numvolumes <- as.character(numvolumes)
  x@edition <- as.character(edition)
  x@volumetitle <- as.character(volumetitle)
  x@seriestitle <- as.character(seriestitle)
  x@seriesvolume <- as.character(seriesvolume)
  x@publisher <- as.character(publisher)
  x@url <- as.character(url)
  x@city <- as.character(city)
  x@state <- as.character(state)
  x@country <- as.character(country)
  x@originallanguage <- as.character(originallanguage)
  x@notes <- as.character(notes)
  x@author <- author
  return(x)
}