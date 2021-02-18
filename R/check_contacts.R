#' @title Check contact information for a record against Neotoma contributors
#' @description Uses the Neotoma API to search and access information about individuals who have contributed to the data in the Neotoma Paleoecology Database
#' @param x contacts A \code{contacts} object associated with a set of names.
#' @param contactname A full or partial name for an individual contributor to the database.
#' @param familyname The full or partial last name for an individual contributor to the database.
#' @param status The current status of the contributor (\code{active} or \code{retired})
#' @export

check_contacts <- function(x) {
  UseMethod('check_contacts')
}

#' @title Get contact information for Neotoma contributors
#' @export
check_contacts.contacts <- function(x, similarity = 0.5, ...) {
  checked <- map(x@contacts, function(y){
    if (is.na(y@contactid)) {
      output <- get_contacts(name = paste0(y@givennames, ' ', 
                                           y@familyname),
                             similarity = similarity, ...)
    } else {
      output <- y
    }
    return(output)
  })
  return(checked)
}