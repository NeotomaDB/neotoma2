#' @importFrom rlang .data
NULL

setClassUnion("id", c("character", "integer", "numeric"))
#' @title An S4 class for multi-contact information from the Neotoma
#' Paleoecology Database.
#' @name contacts_classes
#' @description An unordered list of individual S4 `contact` objects.
#' @export
#' @examples
#' new("contact", familyname = "Goring", givennames = "Simon J.")
#' @returns object of class `contact`
#' @aliases contact-class
#' @md
#' @export
setClass("contact",
         representation(contactid = "id",
                        familyname = "character",
                        leadinginitials = "character",
                        givennames = "character",
                        contactname = "character",
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
                   contactname = NA_character_,
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

#' @aliases contacts-class
#' @rdname contacts_classes
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
#' @importFrom purrr map
#' @examples {
#' simon <- new("contact", familyname = "Goring", givennames = "Simon J.")
#' firstauthor <- new("author", author = simon, order = 1)
#' }
#' @returns object of class `author`
#' @aliases author-class
#' @md
#' @name author_classes
#' @export
setClass("author",
         representation(author = "contact",
                        order = "numeric"),
         prototype(author = new("contact"),
                   order = NA_integer_))

#' @aliases authors-class
#' @rdname author_classes
#' @export
setClass("authors",
         representation(authors = "list"),
         validity = function(object) {
           all(map(object@authors,
                   function(x) {
                     class(x) == "author"}) %>%
             unlist())
         })

#' @title An S4 class for Neotoma `publications`.
#' @name publications_classes
#' @description A publication is liked to an individual Neotoma dataset object
#' They are grouped using an S4 `publications` class.
#' This `publications` class allows a single dataset to have one
#' or more publication classes associated with it.
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
#' @aliases publication-class
#' @md
#' @export
setClass("publication",
         representation(publicationid = "id",
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

#' @aliases publications-class
#' @rdname publications_classes
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

#' @title S4 class for `chronologies` information
#' @name chronologies_classes
#' @description The class for chronologies from the
#' Neotoma Paleoecology Database. A single collection unit may
#' have one or more chronology. The individual chronology
#' classes are grouped into an S4 `chronologies` class.
#' @returns object of class `chronologies`
#' @aliases chronology-class
#' @md
#' @export
setClass("chronology",
         representation(chronologyid = "id",
                        notes = "character",
                        contact = "ANY",
                        agemodel = "character",
                        ageboundolder = "numeric",
                        ageboundyounger = "numeric",
                        isdefault = "logical",
                        dateprepared = "Date",
                        modelagetype = "character",
                        chronologyname = "character",
                        chroncontrols = "ANY"),
         prototype(chronologyid = NA_integer_,
                   notes = NA_character_,
                   contact = list(),
                   agemodel = NA_character_,
                   ageboundolder = NA_integer_,
                   ageboundyounger = NA_integer_,
                   isdefault = FALSE,
                   dateprepared = as.Date(character(1)),
                   modelagetype = NA_character_,
                   chronologyname = NA_character_,
                   chroncontrols = data.frame()))

#' @aliases chronologies-class
#' @rdname chronologies_classes
#' @export
setClass("chronologies",
         representation(chronologies = "list"),
         validity = function(object) {
           all(object@chronologies %>%
                 lapply(class) %>%
                 unlist(recursive = FALSE) ==  "chronology")
         })

#' @title S4 class for `samples` information
#' @name samples_classes
#' @description The standard object class for `samples`
#'  in the Neotoma Paleoecology Database.
#' @returns object of class `sample`
#' @aliases sample-class
#' @md
#' @export
setClass("sample",
         representation(ages = "ANY",
                        igsn = "character",
                        datum = "ANY",
                        depth = "numeric",
                        sampleid = "id",
                        thickness = "numeric",
                        samplename = "character",
                        sampleanalyst = "ANY",
                        analysisunitid = "id",
                        analysisunitname = "character"),
         prototype(ages = list(),
                   igsn = NA_character_,
                   datum = data.frame(),
                   depth = NA_integer_,
                   sampleid = NA_integer_,
                   thickness = NA_integer_,
                   samplename = NA_character_,
                   sampleanalyst = list(),
                   analysisunitid = NA_integer_,
                   analysisunitname = NA_character_))

#' @aliases samples-class
#' @rdname samples_classes
#' @export
setClass("samples",
         representation(samples = "list"))

#' @title S4 class for `repository` information
#' @name repositories_classes
#' @description The standard object class for `repository`
#'  from the Neotoma Paleoecology Database.
#' @returns object of class `repository`
#' @aliases repository-class
#' @md
#' @export
setClass("repository",
         representation(notes = "character",
                        acronym = "character",
                        repository = "character",
                        repositoryid = "id",
                        repositorynotes = "character"),
         prototype(notes = NA_character_,
                   acronym = NA_character_,
                   repository = NA_character_,
                   repositoryid = NA_integer_,
                   repositorynotes = NA_character_))

#' @aliases repositories-class
#' @rdname repositories_classes
#' @export
setClass("repositories",
         representation(repositories = "list"))

#' @title S4 class for `specimens` information
#' @name specimens_classes
#' @description The standard object class for `specimens`
#'  from the Neotoma Paleoecology Database.
#' @returns object of class `specimens`
#' @aliases specimen-class
#' @md
#' @export
setClass("specimen",
         representation(datasetid = "id",
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
                        samplenotes = "character"),
         prototype(datasetid = NA_integer_,
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
                   samplenotes = NA_character_))

#' @aliases specimens-class
#' @rdname specimens_classes
#' @export
setClass("specimens",
         representation(specimens = "list"))

setClassUnion("samplesOrNULL", c("samples", "NULL"))
setClassUnion("specimensOrNULL", c("specimens", "NULL"))

#' @title S4 class for `datasets` information
#' @aliases dataset-class
#' @rdname datasets_classes
#' @description The standard object class for `datasets`
#'  from the Neotoma Paleoecology Database.
#' @export
#' @returns object of class `datasets`
setClass("dataset",
  representation(datasetid = "id",
                 database = "character",
                 doi = "ANY",
                 recdatecreated = "Date",
                 datasettype = "character",
                 datasetname = "character",
                 age_range_old = "numeric",
                 age_range_young = "numeric",
                 age_units = "character",
                 notes = "character",
                 pi_list = "ANY",
                 samples = "samplesOrNULL",
                 specimens = "specimensOrNULL"),
  prototype(datasetid = NA_integer_,
            database = NA_character_,
            doi = list(),
            recdatecreated = as.Date(character(1)),
            datasettype = NA_character_,
            datasetname = NA_character_,
            age_range_old =  NA_integer_,
            age_range_young =  NA_integer_,
            age_units = NA_character_,
            notes = NA_character_,
            pi_list = list(),
            samples = NULL,
            specimens = NULL)
)

#' @aliases datasets-class
#' @rdname datasets_classes
#' @export
setClass("datasets",
         representation(datasets = "list"))

#' @title S4 class for `speleothem` information
#' @name speleothems_classes
#' @description The S4 class for `speleothem` data.
#' @returns object of class `speleothems`
#' @aliases speleothem-class
#' @md
#' @export
setClass("speleothem",
         representation(entityid = "numeric",
                        entityname = "character",
                        siteid = "numeric",
                        collectionunitid = "numeric",
                        dripheight = "numeric",
                        monitoring = "logical",
                        geology = "character",
                        relativeage = "character",
                        speleothemtype = "character",
                        dripheightunits = "character",
                        entitycovertype = "character",
                        entrancedistance = "numeric",
                        entrancedistanceunits = "character",
                        landusecovertype = "character",
                        speleothemdriptype = "character",
                        landusecoverpercent = "numeric",
                        vegetationcovertype = "character",
                        entitycoverthickness = "numeric",
                        vegetationcoverpercent = "numeric"),
         prototype(entityid = NA_integer_,
                   entityname = NA_character_,
                   siteid = NA_integer_,
                   collectionunitid = NA_integer_,
                   dripheight = NA_integer_,
                   monitoring = FALSE,
                   geology = NA_character_,
                   relativeage = NA_character_,
                   speleothemtype = NA_character_,
                   dripheightunits = NA_character_,
                   entitycovertype = NA_character_,
                   entrancedistance = NA_integer_,
                   entrancedistanceunits = NA_character_,
                   landusecovertype = NA_character_,
                   speleothemdriptype = NA_character_,
                   landusecoverpercent = NA_integer_,
                   vegetationcovertype = NA_character_,
                   entitycoverthickness = NA_integer_,
                   vegetationcoverpercent = NA_integer_))

#' @aliases speleothems-class
#' @rdname speleothems_classes
#' @export
setClass("speleothems", representation(speleothems = "list"),
         validity = function(object) {
           all(map(object, function(x) {
             class(x) == "speleothem"
           }) %>%
             unlist())
         })

setClassUnion("datasetsOrNULL", c("datasets", "NULL"))
setClassUnion("speleothemsOrNULL", c("speleothems", "NULL"))
setClassUnion("chronologiesOrNULL", c("chronologies", "NULL"))
#' @title S4 class for `collection units` information.
#' @name collunits_classes
#' @description A `collection unit` represents a collection event from within
#' a `site`. For example, a lake sediment core, or a single dig site within an
#' archaeological site.
#' @returns object of class `collunits`
#' @aliases collunit-class
#' @md
#' @export
setClass("collunit",
         representation(collectionunitid = "id",
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
                        datasets = "datasetsOrNULL",
                        chronologies = "chronologiesOrNULL",
                        defaultchronology = "integer",
                        speleothems = "speleothemsOrNULL"),
         prototype(collectionunitid = NA_integer_,
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
                   defaultchronology = NA_integer_,
                   speleothems = NULL))

#' @aliases collunits-class
#' @rdname collunits_classes
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

setClassUnion("collunitsOrNULL", c("collunits", "NULL"))
#' @title An S4 class for `sites` information
#' @name sites_classes
#' @description The S4 class for sites in the Neotoma Paleoecology Database.
#' @returns object of class `sites`
#' @aliases site-class
#' @md
#' @export
setClass("site",
         representation(siteid = "id",
                        sitename = "character",
                        geography = "sf",
                        altitude = "numeric",
                        geopolitical = "list",
                        area = "numeric",
                        notes = "character",
                        description = "character",
                        collunits = "collunitsOrNULL"),
         prototype(siteid = NA_integer_,
                   sitename = NA_character_,
                   geography = sf::st_sf(sf::st_sfc()),
                   geopolitical = list(),
                   altitude = NA_integer_,
                   area = NA_integer_,
                   notes = NA_character_,
                   description = NA_character_,
                   collunits = NULL))

#' @aliases sites-class
#' @rdname sites_classes
#' @export
setClass("sites",
         representation(sites = "list"),
         validity = function(object) {
           all(map(object@sites, function(x) {
             class(x) == "site"
           }) %>%
             unlist())
         })

#' @title S4 class for taxa information
#' @name taxa_classes
#' @description Taxa details from the Neotoma Paleoecology Database.
#' @returns object of class `taxon`
#' @aliases taxon-class
#' @md
#' @export
setClass("taxon",
         representation(taxonid = "numeric",
                        taxoncode = "character",
                        taxonname = "character",
                        author = "character",
                        ecolgroup = "character",
                        highertaxonid = "numeric",
                        status = "character",
                        taxagroupid = "character",
                        publicationid = "numeric",
                        publication = "character"),
         prototype(taxonid = NA_integer_,
                   taxoncode = NA_character_,
                   taxonname = NA_character_,
                   author = NA_character_,
                   ecolgroup = NA_character_,
                   highertaxonid = NA_integer_,
                   status = NA_character_,
                   taxagroupid = NA_character_,
                   publicationid = NA_integer_,
                   publication = NA_character_))

#' @aliases taxa-class
#' @rdname taxa_classes
#' @export
setClass("taxa", representation(taxa = "list"),
         validity = function(object) {
           all(map(object, function(x) {
             class(x) == "taxon"
           }) %>%
             unlist())
         })