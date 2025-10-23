#' @title set Site Information for Fossil Sites
#' @import lubridate
#' @import sf
#' @importFrom methods new
#' @importFrom methods slot<-
#' @param x Object to be set as a speleothem
#' @param siteid The unique site id for a site.  If this site is new to Neotoma then leave the ID as NA (the default).
#' @param sitename Actual site name as a character string.
#' @param geography An \code{sf} object representing the site location, either as a polygon or point.
#' @param altitude altitude/elevation of the site.
#' @param geopolitical The geopolitical unit in which the site is located.
#' @param area The area of the site or depositional basin in *ha*.  Can be calculated from the polygon.
#' @param description Function to create new site objects for personal analysis. The new object will not be uploaded to the database.
#' @param notes additional information of the site
#' @param collunits Collection units in the site
#' @returns `site` object
#' @export
set_speleothem <- function(x = NA,
                           entityid = NA_integer_,
                           entityname = NA_character_,
                           siteid = NA_integer_,
                           collectionunitid = NA_integer_,
                           datasetid = NA_integer_,
                           dripheight = NA_integer_,
                           monitoring = FALSE,
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
      hash <- digest::digest(uuid::UUIDgenerate(), algo = "xxhash32", serialize = FALSE)
      x@entityid <- as.integer(strtoi(substr(hash, 1, 7), base = 16L))
    } else {
      x@entityid <- entityid
    }
    x$entityname <- entityname
    x$siteid <- siteid
    x$collectionunitid <- collectionunitid
    x$datasetid <- datasetid
    x$dripheight <- dripheight
    x$monitoring <- monitoring
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
      if(length(function_call)>2){
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