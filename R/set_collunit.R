#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @importFrom methods new
#' @importFrom methods slot<-
#' @param x object to be set as collunit
#' @param collectionunitid collection unit identifier
#' @param notes notes
#' @param handle handle
#' @param colldate collection date
#' @param location location of the collection unit
#' @param waterdepth depth at where the sample is taken
#' @param gpslocation location with GPS
#' @param collunittype type of collection unit
#' @param collectiondevice device used to collect the sample
#' @param collectionunitname name of the collection unit
#' @param depositionalenvironment depositional environment
#' @param datasets datasets that the collection unit has
#' @param chronologies chronologies taken from the collection unit
#' @param defaultchronology best chronology model identifier to 
#' be used with this collection unit
#' @export
#' @returns `collunit` object
#' @examples {
#' # Create a collunit
#' my_collunit <- set_collunit(notes = "my lake")
#' }
set_collunit <- function(x = NA,
                         collectionunitid = NA_integer_,
                         notes = NA_character_,
                         handle = NA_character_,
                         colldate = as.Date(character(1)),
                         location = NA_character_,
                         waterdepth = NA_integer_,
                         gpslocation = st_as_sf(st_sfc()),
                         collunittype = NA_character_,
                         collectiondevice = NA_character_,
                         collectionunitname = NA_character_,
                         depositionalenvironment = NA_character_,
                         datasets = new("datasets"),
                         chronologies = new("chronologies"),
                         defaultchronology = NA_integer_) {

  function_call <- match.call()

  if (suppressWarnings(is.na(x))) {
    x <- new("collunit")
    if (is.na(collectionunitid)) {
      x@collectionunitid <- uuid::UUIDgenerate()
    } else {
      x@collectionunitid <- collectionunitid
    }
    x@notes <- notes
    x@handle <- handle
    x@colldate <- colldate
    x@location <- location
    x@waterdepth <- waterdepth
    x@gpslocation <- gpslocation
    x@collunittype <- collunittype
    x@collectiondevice <- collectiondevice
    x@collectionunitname <- collectionunitname
    x@depositionalenvironment <- depositionalenvironment
    x@datasets <- datasets
    x@chronologies <- chronologies
    x@defaultchronology <- defaultchronology

  } else {
    if (is(x, "collunit")) {
      if(length(function_call)>2){
        for (i in 3:length(function_call)) {
          slot(x, names(function_call)[[i]]) <- eval(function_call[[i]])
        }
        return(x)
      } else {
        return(x)
      }
    } else {
      stop("`x` must be a collunit object if it is supplied.")
    }
  }
  return(x)
}