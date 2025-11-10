#' @title Set Speleothem
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom methods is new slot<-
#' @importFrom uuid UUIDgenerate
#' @importFrom digest digest
#' @param x Object to be set as a speleothem
#' @param entityid The speleothem entity ID.
#' @param entityname Name of the speleothem entity.
#' @param siteid The unique site ID for a site.
#' @param collectionunitid The unique collection unit ID for a collection unit.
#' @param dripheight drip height
#' @param monitoring Is the speleothem being monitored? TRUE/FALSE
#' @param geology rock type of the speleothem
#' @param relativeage relative age of the speleothem
#' @param speleothemtype type of speleothem
#' @param dripheightunits Units for drip height
#' @param entitycovertype type of cover around the speleothem
#' @param entrancedistance distance from cave entrance
#' @param entrancedistanceunits Units for distance from cave entrance
#' @param landusecovertype type of land use cover around the speleothem
#' @param speleothemdriptype type of speleothem drip
#' @param landusecoverpercent land use cover percent
#' @param vegetationcovertype type of vegetation cover around the speleothem
#' @param vegetationcoverpercent vegetation cover percent
#' @param entitycoverthickness thickness of the entity cover
#' @returns `speleothem` object
#' @export
set_speleothem <- function(x = NA,
                           entityid = NA_integer_,
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
                           landusecovertype = NA_character_,
                           speleothemdriptype = NA_character_,
                           landusecoverpercent = NA_integer_,
                           vegetationcovertype = NA_character_,
                           entitycoverthickness = NA_integer_,
                           entrancedistanceunits = NA_character_,
                           vegetationcoverpercent = NA_integer_){
  function_call <- match.call()
  if (suppressWarnings(is.na(x))) {
    x <- new("speleothem")
    if (is.na(entityid)) {
      hash <- digest(UUIDgenerate(), algo = "xxhash32", serialize = FALSE)
      x@entityid <- as.integer(strtoi(substr(hash, 1, 7), base = 16L))
    } else {
      x@entityid <- entityid
    }
    x$entityname <- entityname
    x$siteid <- siteid
    x$collectionunitid <- collectionunitid
    x$dripheight <- dripheight
    x$monitoring <- monitoring
    x$geology <- geology
    x$relativeage <- relativeage
    x$speleothemtype <- speleothemtype
    x$dripheightunits <- dripheightunits
    x$entitycovertype <- entitycovertype
    x$entrancedistance <- entrancedistance
    x$landusecovertype <- landusecovertype
    x$speleothemdriptype <- speleothemdriptype
    x$landusecoverpercent <- landusecoverpercent
    x$vegetationcovertype <- vegetationcovertype
    x$entitycoverthickness <- entitycoverthickness
    x$entrancedistanceunits <- entrancedistanceunits
    x@vegetationcoverpercent <- vegetationcoverpercent
  } else {
    if (is(x, "speleothem")) {
      if (length(function_call) > 2) {
        for (i in 3:length(function_call)) {
          slot(x, names(function_call)[[i]]) <- eval(function_call[[i]])
        }
        return(x)
      } else {
        return(x)
      }
    } else {
      stop("`x` must be a site object if it is supplied.")
    }
  }
  return(x)
}