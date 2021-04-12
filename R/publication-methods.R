#' @title An S4 class for Neotoma publications

author <- setClass("author",
                       representation(author = "contact",
                                      order = "numeric"),
                       prototype(author = NULL,
                                 order = NA_integer_))

authors <- setClass("authors",
                   representation(authors = "list"),
                   validity = function(object) {
                     all(map(object@authors, function(x) { 
                       class(x) == "author"}) %>%
                           unlist())
                   })

publication <- setClass("publication",
                    representation(publicationid = "numeric",
                                   publicationtypeid = "numeric",
                                   publicationtype = "character",
                                   year = "character",
                                   citation = "character",
                                   articletitle = "character",
                                   journal = "character",
                                   volume = "character",
                                   issue = "character",
                                   pages = "character",
                                   citationnumber = "character",
                                   doi = "character",
                                   booktitle = "character",
                                   numvolumes = "character",
                                   edition = "character",
                                   volumetitle = "character",
                                   seriestitle = "character",
                                   seriesvolume = "character",
                                   publisher = "character",
                                   url = "character",
                                   city = "character",
                                   state = "character",
                                   country = "character",
                                   originallanguage = "character",
                                   notes = "character",
                                   author = "authors"),
                    prototype(publicationid = NA_integer_,
                              publicationtypeid = NA_integer_,
                              publicationtype = NA_character_,
                              year = NA_character_,
                              citation = NA_character_,
                              articletitle = NA_character_,
                              journal = NA_character_,
                              volume = NA_character_,
                              issue = NA_character_,
                              pages = NA_character_,
                              citationnumber = NA_character_,
                              doi = NA_character_,
                              booktitle = NA_character_,
                              numvolumes = NA_character_,
                              edition = NA_character_,
                              volumetitle = NA_character_,
                              seriestitle = NA_character_,
                              seriesvolume = NA_character_,
                              publisher = NA_character_,
                              url = NA_character_,
                              city = NA_character_,
                              state = NA_character_,
                              country = NA_character_,
                              originallanguage = NA_character_,
                              notes = NA_character_,
                              author = NULL))

setMethod(f = "names",
          signature= signature(x = "publication"),
          definition = function(x){
            slotNames(x)
          })

#' An S4 class for multi-contact information from the Neotoma Paleoecology Database.

publications <- setClass("publications",
                     representation(publications  = "list"),
                     validity = function(object) {
                       all(map(object@publications, function(x) { class(x) == "publication"}) %>%
                             unlist())
                     })

setMethod(f = "show",
          signature= "publications",
          definition = function(object){
            map(object@publications, function(x) {
              data.frame(publicationid = x@publicationid,
                         citation = x@citation,
                         doi = x@doi)
            }) %>% 
              bind_rows() %>%
              print()
          })

setMethod(f = "[[",
          signature= signature(x = "publications", i = "numeric"),
          definition = function(x, i){
            object@publications[[i]]
          })

setMethod(f = "show",
          signature = "publication",
          definition = function(object){
            print(data.frame(publicationid = object@publicationid,
                             citation = object@citation,
                             doi = object@doi))
          })
