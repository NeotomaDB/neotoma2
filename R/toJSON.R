#' @title toJSON
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import sf
#' @importFrom jsonlite toJSON
#' @importFrom geojsonsf sf_geojson
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @importFrom methods is
#' @title Convert sites object to JSON
#' @description
#' Convert a Neotoma2 \code{sites} object into a standardized JSON file for API management.
#' @param x sites R object to be converted
#' @returns The function returns a character string in JSON format
#' @examples {
#' # To find all sites that contain the string "Alexander%"
#' alex.sites <- get_sites(sitename="Alexander%")
#' # Convert the object to json
#' toJSON(alex.sites)
#' }
#' @export
setMethod(f = "toJSON",
          signature = "sites",
          definition = function(x = NA) {

  sample_list <- function(s) {
    assertthat::assert_that(is(s, "sample"),
      msg = "Samples object is empty or not samples")

    output <- list(ages = s@ages,
                    igsn = s@igsn,
                    datum = s@datum,
                    depth = s@depth,
                    sampleid = s@sampleid,
                    thickness = s@thickness,
                    samplename = s@samplename,
                    sampleanalyst = s@sampleanalyst,
                    analysisunitid = s@analysisunitid,
                    analysisunitname = s@analysisunitname)
    return(output)
  }

  specimens_list <- function(sp) {
    assertthat::assert_that(is(sp, "specimen"),
      msg = "Specimens object is empty or not specimens")

    output <- list(datasetid = sp$datasetid,
                    sampleid = sp$sampleid,
                    specimenid = sp$specimenid,
                    repository = list(notes = sp$repository@notes,
                                      acronym = sp$repository@acronym,
                                      repository = sp$repository@repository,
                                      repositoryid = sp$repository@repositoryid,
                                      repositorynotes = sp$repository@repositorynotes),
                    taxonid = sp$taxonid,
                    taxonname = sp$taxonname,
                    elementtype = sp$elementtype,
                    symmetry = sp$symmetry,
                    portion = sp$portion,
                    sex = sp$sex,
                    domesticstatus = sp$domesticstatus,
                    taphonomictype = sp$taphonomictype,
                    nisp = sp$nisp,
                    preservative = sp$preservative,
                    maturity = sp$maturity,
                    samplenotes = sp$samplenotes)

    return(output)
  }

  dataset_list <- function(d) {
    assertthat::assert_that(is(d, "dataset"),
      msg = "Dataset object is empty or not a dataset")

    output <- list(doi = d@doi,
                    agerange = list(agerangeold = d@age_range_old,
                                    agerangeyoung = d@age_range_young),
                    database = d@database,
                    datasetid = d@datasetid,
                    datasetpi = d@pi_list,
                    datasetname = d@datasetname,
                    datasettype = d@datasettype,
                    datasetnotes = d@notes,
                    specimens = purrr::map(d@specimens@specimens, specimens_list),
                    samples = purrr::map(d@samples@samples, sample_list))
    return(output)
  }

  chronology_list <- function(ch) {
    assertthat::assert_that(is(ch, "chronology"),
      msg = "Chronology passed is not a chronology object.")

    output <- list(contact = ch@contact,
                    agemodel = ch@agemodel,
                    agerange = list(ageboundolder = ch@ageboundolder,
                                    ageboundyounger = ch@ageboundyounger),
                    isdefault = ch@isdefault,
                    dateprepared = ch@dateprepared,
                    modelagetype = ch@modelagetype,
                    chronologyname = ch@chronologyname,
                    chronologyid = ch@chronologyid,
                    chroncontrols = ch@chroncontrols)

    return(output)
  }

  collection_list <- function(z) {
    assertthat::assert_that(is(z, "collunit"),
      msg = "Collectionunit passed is not a valid collectionunit object.")

    output <- list(notes = z@notes,
                    handle = z@handle,
                    colldate = z@colldate,
                    location = z@location,
                    waterdepth = z@waterdepth,
                    gpslocation = geojsonsf::sf_geojson(z@gpslocation),
                    collunittype = z@collunittype,
                    collectionunit = z@collectionunitname,
                    collectiondevice = z@collectiondevice,
                    collectionunitid = z@collectionunitid,
                    depositionalenvironment = z@depositionalenvironment,
                    datasets = purrr::map(z@datasets@datasets, dataset_list),
                    chronologies = purrr::map(z@chronologies@chronologies, chronology_list),
                    defaultchronology = z@defaultchronology)
    return(output)
  }

  output <- jsonlite::toJSON(
    purrr::map(x@sites,
                function(y) {
                  list(area = y@area,
                      notes = y@notes,
                      siteid = y@siteid,
                      altitude = y@altitude,
                      sitename = y@sitename,
                      geography = geojsonsf::sf_geojson(y@geography),
                      geopolitical = y@geopolitical,
                      collectionunits = purrr::map(y@collunits@collunits,
                                                    collection_list),
                      sitedescription = y@description)
                }), auto_unbox = TRUE)

  return(output)
})