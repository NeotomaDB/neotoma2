#' @rdname show
#' @export
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

#' @rdname names
#' @export
setMethod(f = "names",
          signature = signature(x = "contact"),
          definition = function(x) {
            slotNames(x)
          })

#' @rdname show
#' @export
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

#' @rdname sub-sub
#' @export
setMethod(f = "[[",
          signature = signature(x = "contacts", i = "numeric"),
          definition = function(x, i) {
            x@contacts[[i]]
          })

#' @rdname cash
#' @export
setMethod(f = "$",
          signature = signature(x = "contact"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @rdname cash
#' @export
setMethod(f = "$",
          signature = signature(x = "contacts"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
                }) %>%
              unlist()
          })

#' @rdname show
#' @export
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

#' @rdname as.data.frame
#' @export
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

#' @rdname as.data.frame
#' @export
setMethod(f = "as.data.frame",
          signature = signature("contacts"),
          definition = function(x) {
            x@contacts %>% map(as.data.frame) %>% 
              bind_rows()
          })

#' @rdname c
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

#' @rdname c
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