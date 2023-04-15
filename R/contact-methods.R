#' @title Show contact object
#' @param object A contact object.
#' @returns null - side effect for printing contact object
setMethod(f = "show",
          signature = "contact",
          definition = function(object) {
            print(data.frame(contactid = testNull(object@contactid),
                             familyname = testNull(object@familyname),
                             givennames = testNull(object@givennames),
                             ORCID = NA,
                             institution = NA,
                             contactstatus = NA,
                             notes = testNull(object@notes)))
          })

#' @title Get names of contacts slots
#' @param x A contact object.
#' @returns names of slots
setMethod(f = "names",
          signature = signature(x = "contact"),
          definition = function(x) {
            slotNames(x)
          })

#' @title Show a contacts object.
#' @param object A contacts object.
#' @returns null - side effect for printing contacts object
setMethod(f = "show",
          signature = "contacts",
          definition = function(object) {
            map(object@contacts, function(x) {
              data.frame(contactid = testNull(x@contactid),
                         familyname = testNull(x@familyname),
                         givennames = testNull(x@givennames),
                         ORCID = NA,
                         institution = NA,
                         contactstatus = NA,
                         notes = testNull(x@notes))
            }) %>%
              bind_rows() %>%
              print()
          })

#' @title Extract or Replace Parts of an Object
#' @param x A contact object.
#' @param i The numeric index of a contact slot.
#' @returns sliced contacts
setMethod(f = "[[",
          signature = signature(x = "contacts", i = "numeric"),
          definition = function(x, i) {
            x@contacts[[i]]
          })

#' @title Extract or Replace Parts of an Object
#' @param x A contact object.
#' @param name The name of a contact slot.
#' @returns value in the selected slot
setMethod(f = "$",
          signature = signature(x = "contact"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title Extract or Replace Parts of an Object
#' @param x A contacts object.
#' @param name The name of a contacts slot.
#' @returns object value from the slot
setMethod(f = "$",
          signature = signature(x = "contacts"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
                }) %>%
              unlist()
          })

#' @title Show a contact object
#' @param object a `contact` object
#' @returns Null - prints a data.frame
setMethod(f = "show",
          signature = "contact",
          definition = function(object) {
            data.frame(contactid = object@contactid,
                       familyname = object@familyname,
                       givennames = object@givennames,
                       ORCID = NA,
                       institution = NA,
                       contactstatus = NA,
                       notes = object@notes) %>%
              print()
          })

#' @title Transform a contacts object to a data.frame()
#' @param x A contact object.
#' @returns `data.frame` object with contact metadata
setMethod(f = "as.data.frame",
          signature = signature("contact"),
          definition = function(x) {
            data.frame(contactid = x@contactid,
                       familyname = x@familyname,
                       givennames = x@givennames,
                       ORCID = NA,
                       institution = NA,
                       contactstatus = NA,
                       notes = x@notes)
          })

#' @title Transform a contacts object to a data.frame()
#' @param x A contacts object.
#' @returns `data.frame` object with multiple `contacts` metadata
setMethod(f = "as.data.frame",
          signature = signature("contacts"),
          definition = function(x) {
            x@contacts %>% map(as.data.frame) %>% bind_rows()
          })

#' @title c Method - Combine contacts objects
#' @param x contacts object 1
#' @param y contacts object 2
#' @importFrom methods is
#' @returns concatenated and clean objects
#' @export
setMethod(f = "c",
          signature = signature(x = "contacts"),
          definition = function(x, y) {
            if (is(y, "contacts")) {
              out <- new("contacts",
                         contacts = unlist(c(x@contacts,
                                              y@contacts),
                                            recursive = FALSE))
            } else if (is(y, "contact")) {
              contactset <- c(x@contacts, y)
              out <- new("contacts", contacts = contactset)
            }
            return(out)
          })

#' @title c Method - Combine contacts objects
#' @param x contacts object 1
#' @param y contacts object 2
#' @returns `contacts` concatenated object
#' @importFrom methods is
#' @export
setMethod(f = "c",
          signature = signature(x = "contact"),
          definition = function(x, y) {
            if (is(y, "contact")) {
              out <- new("contacts",
                         contacts = list(x, y))
            } else if (is(y, "contacts")) {
              contactset <- c(x@contacts, y)
              out <- new("contacts", contacts = contactset)
            }
            return(out)
          })
