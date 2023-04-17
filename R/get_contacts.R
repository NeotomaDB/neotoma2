#' @title Get contact information for Neotoma contributors
#' @importFrom methods new
#' @description Uses the Neotoma API to search and access
#'  information about individuals who have contributed to
#'  the data in the Neotoma Paleoecology Database
#' @param x integer A contact ID
#' @param ...
#' (\code{contactname})  A full or partial name for an individual
#'  contributor to the database.
#' (\code{familyname})  The full or partial last name for an
#'  individual contributor to the database.
#' (\code{status}) The current status of the contributor
#'  (\code{active} or \code{retired})
#' @returns `contacts` object
#' @export
get_contacts <- function(x = NA, ...) {
  UseMethod("get_contacts")
}

#' @title Get contact information for Neotoma contributors
#' @description Uses the Neotoma API to search and access
#'  information about individuals who have contributed to
#'  the data in the Neotoma Paleoecology Database
#' @param x integer A contact ID
#' @param ...
#' (\code{contactname})  A full or partial name for an individual
#'  contributor to the database.
#' (\code{familyname})  The full or partial last name for an
#'  individual contributor to the database.
#' (\code{status}) The current status of the contributor
#'  (\code{active} or \code{retired})
#' @returns `contacts` object
#' @export
get_contacts.numeric <- function(x, ...) {
  if (length(x) > 0) {
    contactname <- paste0(x, collapse = ",")
  }
  
  baseURL <- paste0("data/contacts/", contactname) # nolint
  result <- parseURL(baseURL) %>% cleanNULL() # nolint
  contact <- map(result$data, function(x) {
    x[is.null(x)] <- NA_character_
    new("contact",
        contactid = x$contactid,
        familyname = as.character(x$lastname),
        leadinginitials = NA_character_,
        givennames = as.character(x$firstname),
        suffix = NA_character_,
        ORCID = NA_character_,
        title = NA_character_,
        institution = NA_character_,
        email = as.character(x$email),
        phone = NA_character_,
        contactstatus = NA_character_,
        fax = NA_character_,
        url = as.character(x$url),
        address = as.character(x$address),
        notes = NA_character_) })
  contacts <- new("contacts", contacts = contact)
  return(contacts)
}

#' @title Get contact information for Neotoma contributors
#' @description Uses the Neotoma API to search and access
#'  information about individuals who have contributed to
#'  the data in the Neotoma Paleoecology Database
#' @param x integer A contact ID
#' @param ...
#' (\code{contactname})  A full or partial name for an individual
#'  contributor to the database.
#' (\code{familyname})  The full or partial last name for an
#'  individual contributor to the database.
#' (\code{status}) The current status of the contributor
#'  (\code{active} or \code{retired})
#' @returns `contacts` object
#' @export
get_contacts.default <- function(x, ...) {
  baseURL <- paste0("data/contacts") # nolint
  result <- parseURL(baseURL, ...) %>% cleanNULL() #nolint
  contact <- map(result$data$result, function(x) {
    new("contact",
        contactid = x$contactid,
        familyname = as.character(x$familyname),
        leadinginitials = NA_character_,
        givennames = as.character(x$givennames),
        suffix = NA_character_,
        ORCID = NA_character_,
        title = NA_character_,
        institution = NA_character_,
        email = as.character(x$email),
        phone = NA_character_,
        contactstatus = NA_character_,
        fax = NA_character_,
        url = as.character(x$url),
        address = as.character(x$address),
        notes = NA_character_) })
  contacts <- new("contacts", contacts = contact)
  return(contacts)
}