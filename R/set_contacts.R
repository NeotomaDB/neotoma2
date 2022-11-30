#' @title Set contact information for a new record.
#' @description Create a new contact for a record.
#' Within Neotoma all chronologies have unique numeric identifiers. 
#' Within R, because of the need to use the indentifiers across objects,
#' and because we want to avoid conflicts between naming systems, a 
#' universally unique identifier (UUID) is created for the object ID.
#' @importFrom methods new
#' @importFrom uuid UUIDgenerate
#' @importFrom methods slot<-
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
#' @export

set_contact <- function(contactid = NA_integer_,
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

  if (is.na(contactid)) {
    contactid <- uuid::UUIDgenerate()
  }

  new("contact",
      contactid = contactid,
      familyname = testNull(familyname, NA_character_),
      givennames = testNull(givennames, NA_character_),
      leadinginitials = testNull(leadinginitials, NA_character_),
      suffix = testNull(suffix, NA_character_),
      ORCID = testNull(ORCID, NA_character_),
      title = testNull(title, NA_character_),
      institution = testNull(institution, NA_character_),
      email = testNull(email, NA_character_),
      phone = testNull(phone, NA_character_),
      contactstatus = testNull(contactstatus, NA_character_),
      fax = testNull(fax, NA_character_),
      url = testNull(url, NA_character_),
      address = testNull(address, NA_character_),
      notes = testNull(notes, NA_character_))
}

