#' @title An S4 class for Neotoma contacts

contact <- setClass("contact",
                    representation(contactid = "numeric",
                                   familyname = "character",
                                   leadinginitials = "character",
                                   givennames = "character",
                                   suffix = "character",
                                   ORCID = "character",
                                   title = "character",
                                   institution = "character",
                                   email= "character",
                                   phone = "character",
                                   contactstatus = "character",
                                   fax = "character",
                                   url = "character",
                                   address = "character",
                                   notes = "character"),
                    prototype(contactid = NA_integer_,
                              familyname = NA_character_,
                              leadinginitials = NA_character_,
                              givennames = NA_character_,
                              suffix = NA_character_,
                              ORCID = NA_character_,
                              title = NA_character_,
                              institution = NA_character_,
                              email= NA_character_,
                              phone = NA_character_,
                              contactstatus = NA_character_,
                              fax = NA_character_,
                              url = NA_character_,
                              address = NA_character_,
                              notes = NA_character_))

setMethod(f = "show",
          signature= "contact",
          definition = function(object){
            print(data.frame(contactid = object@contactid,
                             familyname = object@familyname,
                             givennames = object@givennames,
                             ORCID = NA,
                             institution = NA,
                             contactstatus = NA,
                             notes = object@notes))
          })

setMethod(f = "names",
          signature= signature(x = "contact"),
          definition = function(x){
            slotNames(x)
          })

#' An S4 class for multi-contact information from the Neotoma Paleoecology Database.

contacts <- setClass("contacts",
                     representation(contacts  = "list"),
                     validity = function(object) {
                       all(map(object@contacts, function(x) { class(x) == "contact"}) %>%
                             unlist())
                     })

setMethod(f = "show",
          signature= "contacts",
          definition = function(object){
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
          signature= signature(x = "contacts", i = "numeric"),
          definition = function(x, i){
            object@contacts[[i]]
          })

setMethod(f = "$",
          signature= signature(x = "contact"),
          definition = function(x, name){
            slot(x, name)
          })

setMethod(f = "$",
          signature= signature(x = "contacts"),
          definition = function(x, name){
            x %>% 
              map(function(y) { slot(y, name) }) %>% 
              unlist()
          })

setMethod(f = "show",
          signature= "contact",
          definition = function(object){
            data.frame(contactid = object@contactid,
                       familyname = object@familyname,
                       givennames = object@givennames,
                       ORCID = NA,
                       institution = NA,
                       contactstatus = NA,
                       notes = object@notes) %>%
              print()
          })
