#' @aliases names,contact-method
#' @rdname names
setMethod(f = "names",
          signature = signature(x = "contact"),
          definition = function(x) {
            slotNames(x)
          })


#' @aliases show,contacts-method
#' @rdname show
setMethod(f = "show",
          signature = "contacts",
          definition = function(object) {
            as.data.frame(object) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @aliases show,contact-method
#' @rdname show
setMethod(f = "show",
          signature = "contact",
          definition = function(object) {
            as.data.frame(object) %>%
              print(row.names = FALSE)
          })

#' @aliases sub-sub,contacts-method
#' @rdname sub-sub
setMethod(f = "[[",
          signature = signature(x = "contacts", i = "numeric"),
          definition = function(x, i) {
            x@contacts[[i]]
          })

#' @aliases cash,contact-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "contact"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @aliases cash,contacts-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "contacts"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @aliases as.data.frame,contact-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("contact"),
          definition = function(x) {
            data.frame(contactid = testNull(x@contactid),
                       familyname = testNull(x@familyname),
                       givennames = testNull(x@givennames),
                       contactname = testNull(x@contactname),
                       ORCID = testNull(x@ORCID),
                       url = testNull(x@url),
                       contactstatus = testNull(x@contactstatus),
                       notes = testNull(x@notes))
          })

#' @aliases as.data.frame,contacts-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("contacts"),
          definition = function(x) {
            x@contacts %>%
              map(as.data.frame) %>%
              bind_rows()
          })

#' @aliases length,contacts-method
#' @rdname length
setMethod(f = "length",
          signature = signature(x = "contacts"),
          definition = function(x) {
            length(x@contacts)
          })

#' @aliases c,contacts-method
#' @rdname c
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

#' @aliases c,contact-method
#' @rdname c
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