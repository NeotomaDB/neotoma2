#' @title Show contact object
#' @param object A contact object.
setMethod(f = "show",
          signature = "contact",
          definition = function(object) {
            print(data.frame(contactid = object@contactid,
                             familyname = object@familyname,
                             givennames = object@givennames,
                             ORCID = NA,
                             institution = NA,
                             contactstatus = NA,
                             notes = object@notes))
          })

#' @title Get names of contacts slots
#' @param x A contact object.
setMethod(f = "names",
          signature = signature(x = "contact"),
          definition = function(x) {
            slotNames(x)
          })

#' @title Show a contacts object.
#' @param object A contacts object.
setMethod(f = "show",
          signature = "contacts",
          definition = function(object) {
            map(object@contacts, function(x) {
              data.frame(contactid = x@contactid,
                         familyname = x@familyname,
                         givennames = x@givennames,
                         ORCID = NA,
                         institution = NA,
                         contactstatus = NA,
                         notes = x@notes)
            }) %>%
              bind_rows() %>%
              print()
          })

#' @title Extract or Replace Parts of an Object
#' @param x A contact object.
#' @param i The numeric index of a contact slot.
setMethod(f = "[[",
          signature = signature(x = "contacts", i = "numeric"),
          definition = function(x, i) {
            object@contacts[[i]]
          })

#' @title Extract or Replace Parts of an Object
#' @param x A contact object.
#' @param name The name of a contact slot.
setMethod(f = "$",
          signature = signature(x = "contact"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title Extract or Replace Parts of an Object
#' @param x A contacts object.
#' @param name The name of a contacts slot.
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
setMethod(f = "as.data.frame",
          signature = signature("contacts"),
          definition = function(x) {
            x@contacts %>% map(as.data.frame) %>% bind_rows()
          })
