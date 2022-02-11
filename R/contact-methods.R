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

setMethod(f = "names",
          signature = signature(x = "contact"),
          definition = function(x) {
            slotNames(x)
          })

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

setMethod(f = "[[",
          signature = signature(x = "contacts", i = "numeric"),
          definition = function(x, i) {
            object@contacts[[i]]
          })

setMethod(f = "$",
          signature = signature(x = "contact"),
          definition = function(x, name) {
            slot(x, name)
          })

setMethod(f = "$",
          signature = signature(x = "contacts"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
                }) %>%
              unlist()
          })

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

setMethod(f = "as.data.frame",
          signature = signature("contacts"),
          definition = function(x) {
            x@contacts %>% map(as.data.frame) %>% bind_rows()
          })
