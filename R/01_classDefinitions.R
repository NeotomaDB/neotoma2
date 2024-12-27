setClassUnion("id", c("character", "integer", "numeric"))

#' @title An S4 class for Neotoma contacts
#' @description The object that contains the contact information for an
#' individual, along with associated metadata.
#' @export
#' @examples
#' new("contact", familyname = "Goring", givennames = "Simon J.")
#' @returns object of class `contact`
setClass("contact",
                    representation(contactid = "id",
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

#' @title An S4 class for multi-contact information from the Neotoma
#' Paleoecology Database.
#' @description An unordered list of individual S4 `contact` objects.
#' @examples {
#' # Create two contact objects and associate them within a contacts object.
#' simon <- new("contact", familyname = "Goring", givennames = "Simon J.")
#' socorro <- new("contact", familyname = "Dominguez", givennames = "Socorro")
#' packagers <- new("contacts", contacts = list(simon, socorro))
#' packagers
#' }
#' @import dplyr
#' @importFrom purrr map
#' @returns object of class `contacts`
#' @export
setClass("contacts",
                     representation(contacts  = "list"),
                     validity = function(object) {
                       all(map(object@contacts, function(x) {
                         class(x) == "contact"
                       }) %>%
                         unlist())
                     })

#' @title An S4 class for the authors of a Neotoma publication.
#' @description This class combines the S4 class `contact` with a numeric
#' author order. This allows us to reuse `contact` objects, and to assign
#' the authorship order within a publication. The full set of authors for
#' a publication are represented by the `authors` object.
#' @examples {
#' simon <- new("contact", familyname = "Goring", givennames = "Simon J.")
#' firstauthor <- new("author", author = simon, order = 1)
#' }
#' @returns object of class `author`
#' @export
setClass("author",
                   representation(author = "contact",
                                  order = "numeric"),
                   prototype(author = new("contact"),
                             order = NA_integer_))

#' @title An S4 class for a set of Neotoma author objects.
#' @description The S4 `authors` are a set of individual `author` objects that
#' are then associated with a single S4 `publication` class.
#' @examples {
#' simon <- new("contact", familyname = "Goring", givennames = "Simon J.")
#' socorro <- new("contact", familyname = "Dominguez", givennames = "Socorro")
#' first_author <- new("author", author = simon, order = 1)
#' second_author <- new("author", author = socorro, order = 2)
#' author_list <- new("authors", authors = list(first_author, second_author))
#' }
#' @returns object of class `authors`
#' @export
setClass("authors",
                    representation(authors = "list"),
                    validity = function(object) {
                      all(map(object@authors, function(x) {
                        class(x) == "author"}) %>%
                          unlist())
                    })

#' @title An S4 class for a single Neotoma publication.
#' @description A publication is liked to an individual Neotoma dataset object
#' They are grouped using an S4 `publications` class.
#' @examples {
#' simon <- new("contact", familyname = "Goring", givennames = "Simon J.")
#' socorro <- new("contact", familyname = "Dominguez", givennames = "Socorro")
#' first_author <- new("author", author = simon, order = 1)
#' second_author <- new("author", author = socorro, order = 2)
#' author_list <- new("authors", authors = list(first_author, second_author))
#' pub <- new("publication",
#'            articletitle = "Top or bottom: Best toast spreading surfaces.",
#'            journal = "Peanut Butter Science",
#'            year = "2022",
#'            volume = "2",
#'            author = author_list)
#'            }
#' @returns object of class `publication`
#' @export
setClass("publication", representation(publicationid = "id",
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
                                  author = new("authors")))

#' @title An S4 class for multi-publication information from the Neotoma 
#' Paleoecology Database. This S4 class allows a single dataset to have one
#' or more publication classes associated with it.
#' @returns object of class `publications`
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
#' @description The class for chronologies from the
#' Neotoma Paleoecology Database. A single collection unit may
#' have one or more chronology. These individual chronology
#' classes are then grouped into an S4 `chronologies` class.
#' @returns object of class `chronology`
#' @export
setClass(
  # Set the name for the class
  "chronology",
  # Define the slots
  slots = c(chronologyid = "id",
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
                   dateprepared = as.Date(character(1)),
                   modelagetype = NA_character_,
                   chronologyname = NA_character_,
                   chroncontrols = data.frame()),
)

#' @title S4 class for chronologies information
#' @description The grouped class for chronologies
#'  from the Neotoma Paleoecology Database.
#' @returns object of class `chronologies` 
#' @export
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
#' @returns object of class `sample`
#' @export
setClass(
  # Set the name for the class
  "sample",
  # Define the slots
  slots = c(ages = "ANY",
            igsn = "character",
            datum = "ANY",
            depth = "numeric",
            sampleid = "id",
            thickness = "numeric",
            samplename = "character",
            sampleanalyst = "ANY",
            analysisunitid = "id",
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
#' @returns object of class `samples`
#' @export
setClass(
  # Set the name for the class
  "samples",
  slots = c(samples = "list"))

#' @title S4 class for repository information
#' @description The standard object class for repository
#'  from the Neotoma Paleoecology Database.
#' @returns object of class `repository`
#' @export
setClass(
  # Set the name for the class
  "repository",
  # Define the slots
  slots = c(notes = "character",
            acronym = "character",
            repository = "character",
            repositoryid = "id",
            repositorynotes = "character"
  ),
  # Set the default values for the slot
  prototype = list(notes = NA_character_,
                   acronym = NA_character_,
                   repository = NA_character_,
                   repositoryid = NA_integer_,
                   repositorynotes = NA_character_
  ),
)

#' @title S4 class for repositories information
#' @description The grouped class for repositories from
#'  the Neotoma Paleoecology Database.
#' @returns object of class `repositories`
#' @export
setClass(
  # Set the name for the class
  "repositories",
  slots = c(repositories = "list"))

#' @title S4 class for specimen information
#' @description The standard object class for specimen
#'  from the Neotoma Paleoecology Database.
#' @returns object of class `specimen`
#' @export
setClass(
  # Set the name for the class
  "specimen",
  # Define the slots
  slots = c(datasetid = "id",
            sampleid = "id",
            specimenid = "numeric",
            repository = "repository",
            taxonid = "id",
            taxonname = "character",
            elementtype = "character",
            symmetry = "character",
            portion = "character",
            sex = "character",
            domesticstatus = "character",
            taphonomictype = "character",
            nisp = "numeric",
            preservative = "character",
            maturity = "character",
            samplenotes = "character"
  ),
  # Set the default values for the slot
  prototype = list(datasetid = NA_integer_,
                   sampleid = NA_integer_,
                   specimenid = NA_integer_,
                   repository = NULL,
                   taxonid = NA_integer_,
                   taxonname = NA_character_,
                   elementtype = NA_character_,
                   symmetry = NA_character_,
                   portion = NA_character_,
                   sex = NA_character_,
                   domesticstatus = NA_character_,
                   taphonomictype = NA_character_,
                   nisp = NA_integer_,
                   preservative = NA_character_,
                   maturity = NA_character_,
                   samplenotes = NA_character_
  ),
)

#' @title S4 class for specimens information
#' @description The grouped class for specimens from
#'  the Neotoma Paleoecology Database.
#' @returns object of class `specimens`
#' @export
setClass(
  # Set the name for the class
  "specimens",
  slots = c(specimens = "list"))

#' @title S4 class for dataset information
#' @description The standard object class for datasets
#'  from the Neotoma Paleoecology Database.
#' @export
#' @returns object of class `dataset`
setClass(
  # Set the name for the class
  "dataset",
  # Define the slots
  slots = c(datasetid = "id",
            database = "character",
            doi = "ANY",
            datasettype = "character",
            datasetname = "character",
            age_range_old = "numeric",
            age_range_young = "numeric",
            age_units = "character",
            notes = "character",
            pi_list = "ANY",
            samples = "samples",
            specimens = "specimens"),
  # Set the default values for the slot
  prototype = list(datasetid = NA_integer_,
                   database = NA_character_,
                   doi = list(),
                   datasettype = NA_character_,
                   datasetname = NA_character_,
                   age_range_old =  NA_integer_,
                   age_range_young =  NA_integer_,
                   age_units = NA_character_,
                   notes = NA_character_,
                   pi_list = list(),
                   samples = NULL,
                   specimens = NULL),
)

#' @title S4 class for datasets information
#' @description The grouped class for datasets from
#'  the Neotoma Paleoecology Database.
#' @returns object of class `datasets`
#' @export
setClass(
  # Set the name for the class
  "datasets",
  slots = c(datasets = "list"))

#' @title S4 class for collection units information.
#' @description A collection unit represents a collection event from within 
#' a site. For example, a lake sediment core, or a single dig site within an
#' archaeological site.
#' @returns object of class `collunit`
#' @export
setClass(
  # Set the name for the class
  "collunit",
  slots = c(collectionunitid = "id",
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
            chronologies = "chronologies",
            defaultchronology = "integer"),
  prototype = list(collectionunitid = NA_integer_,
                   notes = NA_character_,
                   handle = NA_character_,
                   colldate = as.Date(character(1)),
                   location = NA_character_,
                   waterdepth = NA_integer_,
                   gpslocation = sf::st_as_sf(sf::st_sfc()),
                   collunittype = NA_character_,
                   collectiondevice = NA_character_,
                   collectionunitname = NA_character_,
                   depositionalenvironment = NA_character_,
                   datasets = NULL,
                   chronologies = NULL,
                   defaultchronology = NA_integer_))


#' @title An S4 class for Neotoma Collection Units
#' @description Holds Collection unit information
#'  from the Neotoma Paleoecology Database.
#'  @returns object of class `collunits`
#' @export
setClass("collunits",
         representation(collunits = "list"),
         validity = function(object) {
           all(map(object@collunits,
                   function(x) {
                     class(x) == "collunit"
                   }) %>%
                 unlist())
         })

#' @title An S4 class for site information
#' @description The standard object class for sites
#'  from the Neotoma Paleoecology Database.
#' @import sf
#' @returns object of class `site`
#' @export
setClass(
  # Set the name for the class
  "site",
  # Define the slots
  slots = c(siteid = "id",
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

#' @title An S4 class for multi-site information 
#' @description The standard object class for multi-sites
#'  from the Neotoma Paleoecology Database.from
#'  @returns object of class `sites`
setClass("sites",
                  representation(sites = "list"),
                  validity = function(object) {
                    all(map(object@sites, function(x) {
                      class(x) == "site"
                    }) %>%
                      unlist())
                  })

#' @title S4 class for specimen information
#' @description Taxon class for single taxon information
#'  from the Neotoma Paleoecology Database.
#' @returns object of class `taxon`
#' @export
setClass("taxon",
         slots = c(taxonid = "numeric",
                   taxoncode = "character",
                   taxonname = "character",
                   author = "character",
                   ecolgroup = "character",
                   highertaxonid = "numeric",
                   status = "character",
                   taxagroupid = "character",
                   publicationid = "numeric",
                   publication = "character"),
         prototype = list(taxonid = NA_integer_,
                   taxoncode = NA_character_,
                   taxonname = NA_character_,
                   author = NA_character_,
                   ecolgroup = NA_character_,
                   highertaxonid = NA_integer_,
                   status = NA_character_,
                   taxagroupid = NA_character_,
                   publicationid = NA_integer_,
                   publication = NA_character_))

#' @title S4 class for taxa information
#' @description Taxa class for taxa information
#'  from the Neotoma Paleoecology Database.
#' @returns object of class `taxon`
#' @export
setClass("taxa", representation(taxa = "list"),
         validity = function(object) {
           all(map(object, function(x) {
             class(x) == "taxon"
           }) %>%
             unlist())
         })
