#' @title Check contact information for a record against Neotoma contributors
#' @description
#'   Uses the Neotoma API to search and access information about individuals
#'   who have contributed to the data in the Neotoma Paleoecology Database
#' @param x contacts A \code{contacts} object associated with a set of names.
#' @param ... Additional parameters associated with the call.
#' @return contacts object
#' @export

check_contacts <- function(x, ...) {
  if (class(x) %in% c("contacts")) {
    UseMethod("check_contacts")
  } else {
    stop("check_contacts() must operate on a `contacts` or `contact` object.")
  }
}

#' @title Get contact information for Neotoma contributors
#' @param x contacts A \code{contacts} object associated with a set of names.
#' @param similarity The similarity score between matched records (from 0 - 1).
#' @param ... Additional parameters associated with the call.
#' @returns contacts object
#' @export
check_contacts.contacts <- function(x, similarity = 0.5, ...) {
  checked <- map(x@contacts, function(y) {
    if (is.na(y@contactid) | !is(y@contactid, "numeric")) {
      output <- get_contacts(name = paste0(y@givennames, " ",
                                           y@familyname),
                             similarity = similarity, ...)
    } else {
      output <- y
    }
    return(output)
  })
  return(checked)
}