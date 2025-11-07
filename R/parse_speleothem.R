#' @title parse_speleothem
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @importFrom methods new
#' @description An internal helper function used to parse the Neotoma API
#' content of speleothems into `neotoma2R` objects.
#' @param data The API response.
#' @returns `list` with cleaned and parsed data from HTTP request
#' @keywords internal
#' @noRd
parse_speleothem <- function(data) {
  speleothems <- map(data, function(x) {
    x <- x$speleothem
    if (x$entrancedistanceunits == 37) {
      x$entrancedistanceunits <- "m"
    }
    sp <-
      list(entityid = x$entityid,
           entityname = x$entityname,
           siteid = x$siteid,
           collectionunitid = x$collectionunitid,
           datasetid = x$datasetid,
           dripheight = use_na(x$dripheight, "int"),
           monitoring = use_na(x$monitoring, "bool"),
           geology = use_na(x$geology, "char"),
           relativeage = use_na(x$rockage, "char"),
           speleothemtype = use_na(x$speleothemtype, "char"),
           dripheightunits = use_na(x$dripheightunits, "char"),
           entitycovertype = use_na(x$entitycovertype, "char"),
           entrancedistance = use_na(x$entrancedistance, "int"),
           entrancedistanceunits = use_na(x$entrancedistanceunits, "char"),
           landusecovertype = use_na(x$landusecovertype, "char"),
           speleothemdriptype = use_na(x$speleothemdriptype, "char"),
           landusecoverpercent = use_na(x$landusecoverpercent, "int"),
           vegetationcovertype = use_na(x$vegetationcovertype, "char"),
           entitycoverthickness = use_na(x$entitycoverthickness, "int"),
           vegetationcoverpercent = use_na(x$vegetationcoverpercent, "int"))
    do.call(build_speleothem, sp)
  })
  if (is.null(speleothems) || all(sapply(speleothems, is.null))) {
    speleo <- new("speleothems", speleothems = list())
  } else {
    speleo <- new("speleothems", speleothems = speleothems)
  }
  return(speleo)
}