#' @title An S4 class for Neotoma contacts
setClass("contact",
                    representation(contactid = "numeric",
                                   familyname = "character",
                                   leadinginitials = "character",
                                   givennames = "character",
                                   suffix = "character",
                                   ORCID = "character",
                                   title = "character",
                                   institution = "character",
                                   email = "character",
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
                              email = NA_character_,
                              phone = NA_character_,
                              contactstatus = NA_character_,
                              fax = NA_character_,
                              url = NA_character_,
                              address = NA_character_,
                              notes = NA_character_))

#' An S4 class for multi-contact information
#'  from the Neotoma Paleoecology Database.
setClass("contacts",
                     representation(contacts  = "list"),
                     validity = function(object) {
                       all(map(object@contacts, function(x) {
                         class(x) == "contact"
                       }) %>%
                         unlist())
                     })

#' @title An S4 class for Neotoma publications
#' @export
setClass("author",
                   representation(author = "contact",
                                  order = "numeric"),
                   prototype(author = NULL,
                             order = NA_integer_))

#' @title An S4 class for a set of Neotoma author objects.
#' @export
setClass("authors",
                    representation(authors = "list"),
                    validity = function(object) {
                      all(map(object@authors, function(x) {
                        class(x) == "author"}) %>%
                          unlist())
                    })

#' @title An S4 class for a single Neotoma publication.
#' @export
setClass("publication",
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

#' @title
#'  An S4 class for multi-publication information
#'  from the Neotoma Paleoecology Database.
#' @export
setClass("publications",
                         representation(publications  = "list"),
                         validity = function(object) {
                           all(map(object@publications,
                                   function(x) {
                                     class(x) == "publication"
                                   }) %>%
                                 unlist())
                         })

#' @title S4 class for chronologies information
#' @description The grouped class for chronologies
#'  from the Neotoma Paleoecology Database.
setClass(
  # Set the name for the class
  "chronology",
  # Define the slots
  slots = c(chronologyid = "numeric",
            notes = "character",
            contact = "ANY",
            agemodel = "character",
            ageboundolder = "numeric",
            ageboundyounger = "numeric",
            isdefault = "numeric",
            dateprepared = "Date",
            modelagetype = "character",
            chronologyname = "character",
            chroncontrols = "ANY"),
  # Set the default values for the slot
  prototype = list(chronologyid = NA_integer_,
                   notes = NA_character_,
                   contact = list(),
                   agemodel = NA_character_,
                   ageboundolder = NA_integer_,
                   ageboundyounger = NA_integer_,
                   isdefault = NA_integer_,
                   dateprepared = as.Date(character(0)),
                   modelagetype = NA_character_,
                   chronologyname = NA_character_,
                   chroncontrols = data.frame()),
)

#' @title S4 class for chronologies information
#' @description The grouped class for chronologies
#'  from the Neotoma Paleoecology Database.
setClass(
  "chronologies",
  slots = c(chronologies = "list"),
  # Validity functions
  validity = function(object) {
    all(object@chronologies %>%
          lapply(class) %>%
          unlist(recursive = FALSE) ==  "chronology")
  })

#' @title S4 class for dataset information
#' @description The standard object class for samples
#'  from the Neotoma Paleoecology Database.
#' @import sf
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
setClass(
  # Set the name for the class
  "sample",
  # Define the slots
  slots = c(ages = "ANY",
            igsn = "character",
            datum = "ANY",
            depth = "numeric",
            sampleid = "numeric",
            thickness = "numeric",
            samplename = "character",
            sampleanalyst = "ANY",
            analysisunitid = "numeric",
            analysisunitname = "character"),

  # Set the default values for the slot
  prototype = list(ages = list(),
                   igsn = NA_character_,
                   datum = data.frame(),
                   depth = NA_integer_,
                   sampleid = NA_integer_,
                   thickness = NA_integer_,
                   samplename = NA_character_,
                   sampleanalyst = list(),
                   analysisunitid = NA_integer_,
                   analysisunitname = NA_character_),
)

#' @title S4 class for the set of samples
#' @description The grouped class for samples from
#'  the Neotoma Paleoecology Database.
setClass(
  # Set the name for the class
  "samples",
  slots = c(samples = "list"))

#' @title S4 class for dataset information
#' @description The standard object class for datasets
#'  from the Neotoma Paleoecology Database.
setClass(
  # Set the name for the class
  "dataset",
  # Define the slots
  slots = c(datasetid = "numeric",
            database = "character",
            doi = "ANY",
            datasettype = "character",
            age_range_old = "numeric",
            age_range_young = "numeric",
            notes = "character",
            pi_list = "ANY",
            samples = "samples"),
  # Set the default values for the slot
  prototype = list(datasetid = NA_integer_,
                   database = NA_character_,
                   doi = list(),
                   datasettype = NA_character_,
                   age_range_old =  NA_integer_,
                   age_range_young =  NA_integer_,
                   notes = NA_character_,
                   pi_list = list(),
                   samples = NULL),
)

#' @title S4 class for datasets information
#' @description The grouped class for datasets from
#'  the Neotoma Paleoecology Database.
setClass(
  # Set the name for the class
  "datasets",
  slots = c(datasets = "list"))

#' @title S4 class for collunits information
setClass(
  # Set the name for the class
  "collunit",
  slots = c(collectionunitid = "numeric",
            notes = "character",
            handle = "character",
            colldate = "Date",
            location = "character",
            waterdepth = "numeric",
            gpslocation = "sf",
            collunittype = "character",
            collectiondevice = "character",
            collectionunitname = "character",
            depositionalenvironment = "character",
            datasets = "datasets",
            chronologies = "chronologies"),
  prototype = list(collectionunitid = NA_integer_,
                   notes = NA_character_,
                   handle = NA_character_,
                   colldate = as.Date(character(0)),
                   location = NA_character_,
                   waterdepth = NA_integer_,
                   gpslocation = sf::st_as_sf(sf::st_sfc()),
                   collunittype = NA_character_,
                   collectiondevice = NA_character_,
                   collectionunitname = NA_character_,
                   depositionalenvironment = NA_character_,
                   datasets = NULL,
                   chronologies = NULL))

#' An S4 class for Neotoma Collection Units
#' @description Holds Collection unit information
#'  from the Neotoma Paleoecology Database.
#' @importFrom purrr map
setClass("collunits",
         representation(collunits = "list"),
         validity = function(object) {
           all(map(object@collunits,
                   function(x) {
                     class(x) == "collunit"
                   }) %>%
                 unlist())
         })

#' An S4 class for site information from the Neotoma Paleoecology Database.
#' @import sf
setClass(
  # Set the name for the class
  "site",
  # Define the slots
  slots = c(siteid = "numeric",
            sitename = "character",
            geography = "sf",
            altitude = "numeric",
            geopolitical = "list",
            area = "numeric",
            notes = "character",
            description = "character",
            collunits = "collunits"),
  # Set the default values for the slot
  prototype = list(siteid = NA_integer_,
                   sitename = NA_character_,
                   geography = sf::st_sf(sf::st_sfc()),
                   geopolitical = list(),
                   altitude = NA_integer_,
                   area = NA_integer_,
                   notes = NA_character_,
                   description = NA_character_,
                   collunits = NULL) # check what would really be a NA here
  # Add a validity function that can test data consistency.
  # This is not called if you have an initialize function defined!
)

#' An S4 class for multi-site information from
#'  the Neotoma Paleoecology Database.
setClass("sites",
                  representation(sites = "list"),
                  validity = function(object) {
                    all(map(object@sites, function(x) {
                      class(x) == "site"
                    }) %>%
                      unlist())
                  })

