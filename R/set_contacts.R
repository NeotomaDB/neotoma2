#' @title Set contact information for a new record.
#' @description Create a new contact for a record.
#' Within Neotoma all chronologies have unique numeric identifiers. 
#' Within R, because of the need to use the indentifiers across objects,
#' and because we want to avoid conflicts between naming systems, a 
#' universally unique identifier (UUID) is created for the object ID.
#' @importFrom methods new
#' @importFrom uuid UUIDgenerate
#' @importFrom methods slot<-
#' @param x Object to be set as a contact
#' @param contactid An arbitrary Contact identification number.
#' @param familyname Family or surname name of a person.
#' @param leadinginitials Leading initials for given or forenames
#' without spaces (e.g. \code{G.G.}).
#' @param givennames Given or forenames of a person (e.g. 
#' \code{George Gaylord}). Initials with spaces are used if full
#' given names are not known (e.g. \code{G. G}).
#' @param suffix Suffix of a person’s name (e.g. \code{Jr.}, \code{III}).
#' @param ORCID A unique ORCID (see \url{https://orcid.org}).
#' @param title A person’s title (e.g. \code{Dr.}, \code{Prof.},
#' \code{Prof. Dr}).
#' @param institution The institution where an individual works.
#' @param email An individuals email address
#' @param phone Their phone number
#' @param contactstatus Are they "active" or "retired"?
#' @param fax Do people still use fax machines?
#' @param url Their homepage
#' @param address A physical address
#' @param notes Notes about the individual
#' @returns `contact` object
#' @export

set_contact <- function(x = NA,
                        contactid = NA_integer_,
                        familyname = NA_character_,
                        leadinginitials = NA_character_,
                        givennames = NA_character_,
                        suffix = NA_character_,
                        ORCID = NA_character_,  # nolint
                        title = NA_character_,
                        institution = NA_character_,
                        email = NA_character_,
                        phone = NA_character_,
                        contactstatus = NA_character_,
                        fax = NA_character_,
                        url = NA_character_,
                        address = NA_character_,
                        notes = NA_character_) {
  
  function_call <- match.call()
  
  if (suppressWarnings(is.na(x))) {
    x <- new("contact")
    if (is.na(contactid)) {
      x@contactid <- uuid::UUIDgenerate()
    } else {
      x@contactid <- contactid
    }
    x@familyname <- familyname
    x@givennames <- givennames
    x@leadinginitials <- leadinginitials
    x@suffix <- suffix
    x@ORCID <- ORCID
    x@title <- title
    x@institution <- institution
    x@email <- email
    x@phone <- phone
    x@contactstatus <- contactstatus
    x@fax <- fax
    x@url <- url
    x@address <- address
    x@notes <- notes

  } else {
    if (is(x, "contact")) {
      if(length(function_call)>2){
        for (i in 3:length(function_call)) {
          slot(x, names(function_call)[[i]]) <- eval(function_call[[i]])
        }
        return(x)
      } else {
        return(x)
      }
    } else {
      stop("`x` must be a contact object if it is supplied.")
    }
  }
  return(x)
}