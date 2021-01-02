#' @title Get contact information for Neotoma contributors
#' @description Uses the Neotoma API to search and access information about individuals who have contributed to the data in the Neotoma Paleoecology Database
#' @param x integer A contact ID
#' @param contactname A full or partial name for an individual contributor to the database.
#' @param familyname The full or partial last name for an individual contributor to the database.
#' @param status The current status of the contributor (\code{active} or \code{retired})
#' @export

get_contacts <- function(x, ...) {
  UseMethod('get_contacts')
}

#' @title Get contact information for Neotoma contributors
#' @importFrom methods new
#' @export
get_contacts.numeric <- function(x, ...) {

  if (length(x) > 0) {
    contactname <- paste0(contactname, collapse = ',')
  }

  baseURL <- paste0('data/contacts/', contactname)

  result <- parseURL(baseURL)

  contact <- map(result$data[[1]], function(x) {
                  new("contact",
                      contactid = x$contactid,
                      familyname = x$lastname,
                      leadinginitials = NA_character_,
                      givennames = x$firstname,
                      suffix = NA_character_,
                      ORCID = NA_character_,
                      title = NA_character_,
                      institution = NA_character_,
                      email= x$email,
                      phone = NA_character_,
                      contactstatus = NA_character_,
                      fax = NA_character_,
                      url = x$url,
                      address = x$address,
                      notes = NA_character_) })
  return(contact)
}
