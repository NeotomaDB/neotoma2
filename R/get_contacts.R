#' @title Get `contact` information for Neotoma contributors
#' @name get_contacts
#' @author Simon Goring \email{goring@wisc.edu}
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom methods new
#' @param x integer A contact ID
#' @param ...
#' (\code{contactname})  A full or partial name for an individual
#'  contributor to the database.
#' (\code{familyname})  The full or partial last name for an
#'  individual contributor to the database.
#' (\code{status}) The current status of the contributor
#' (\code{active} or \code{retired})
#' @returns `contacts` object
#' @description Uses the Neotoma API to search and access
#'  information about individuals who have contributed to
#'  the data in the Neotoma Paleoecology Database
#' @md
#' @export
get_contacts <- function(x = NA, ...) {
  if (missing(x)) {
    UseMethod("get_contacts", "default")
  } else {
    UseMethod("get_contacts", x)
  }
}


#' @rdname get_contacts
#' @method get_contacts numeric
#' @exportS3Method get_contacts numeric
get_contacts.numeric <- function(x, ...) {
  if (length(x) > 0) {
    contactname <- paste0(x, collapse = ",")
  }
  baseURL <- paste0("data/contacts/", contactname)
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  result <- result %>% cleanNULL()
  contact <- map(result$data,
                 function(x) {
                   x[is.null(x)] <- NA_character_
                   new("contact",
                       contactid = use_na(x$contactid, "int"),
                       familyname = use_na(x$familyname, "char"),
                       leadinginitials = use_na(x$leadinginitials,
                                                "char"),
                       givennames = use_na(x$givennames, "char"),
                       contactname = use_na(x$contactname, "char"),
                       suffix = use_na(x$suffix, "char"),
                       ORCID = use_na(x$ORCID, "char"),
                       title = use_na(x$title, "char"),
                       institution = use_na(x$institution, "char"),
                       email = use_na(x$email, "char"),
                       phone = use_na(x$phone, "char"),
                       contactstatus = use_na(x$contactstatus,
                                              "char"),
                       fax = use_na(x$fax, "char"),
                       url = use_na(x$url, "char"),
                       address = use_na(x$address, "char"),
                       notes = use_na(x$notes, "char"))
                 })
  contacts <- new("contacts", contacts = contact)
  return(contacts)
}

#' @rdname get_contacts
#' @method get_contacts default
#' @exportS3Method get_contacts default
get_contacts.default <- function(x, ...) {
  baseURL <- paste0("data/contacts")
  result <- tryCatch(
    parseURL(baseURL, ...),
    error = function(e) {
      stop("API call failed: ", e$message)
      NULL
    }
  )
  params <- get_params("contacts")
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  cl <- lapply(cl, eval, envir = parent.frame())
  if (!all(names(cl) %in% params)) {
    warning("Some parameters seem invalid. 
             The current accepted parameters are: ",
            paste(unlist(params), collapse = ", "))
  }
  result <- result %>% cleanNULL()
  contact <- map(result$data,
                 function(x) {
                   new("contact",
                       contactid = use_na(x$contactid, "int"),
                       familyname = use_na(x$familyname, "char"),
                       leadinginitials = use_na(x$leadinginitials,
                                                "char"),
                       givennames = use_na(x$givennames, "char"),
                       contactname = use_na(x$contactname, "char"),
                       suffix = use_na(x$suffix, "char"),
                       ORCID = use_na(x$ORCID, "char"),
                       title = use_na(x$title, "char"),
                       institution = use_na(x$institution, "char"),
                       email = use_na(x$email, "char"),
                       phone = use_na(x$phone, "char"),
                       contactstatus = use_na(x$contactstatus,
                                              "char"),
                       fax = use_na(x$fax, "char"),
                       url = use_na(x$url, "char"),
                       address = use_na(x$address, "char"),
                       notes = use_na(x$notes, "char"))
                 })
  contacts <- new("contacts", contacts = contact)
  return(contacts)
}