#' @md
#' @title parseURL
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @import stringr
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @keywords internal
#' @noRd
parse_speleothem <- function(result) {
  data <- result$data
  speleothems <- purrr::map(data, function(x) {
   sp <-  list(entityid = x$speleothem$entityid,
         entityname = x$speleothem$entityname,
         siteid = x$speleothem$siteid,
         collectionunitid = x$speleothem$collectionunitid,
         datasetid = x$speleothem$datasetid,
         dripheight = use_na(testNull(x$speleothem$dripheight, NA), "int"),
         monitoring = use_na(x$speleothem$monitoring, "bool"),
         relativeage = use_na(testNull(x$speleothem$relativeage, NA), "char"),
         speleothemtype = use_na(testNull(x$speleothem$speleothemtype, NA), "char"),
         dripheightunits = use_na(testNull(x$speleothem$dripheightunits, NA), "char"),
         entitycovertype = use_na(testNull(x$speleothem$entitycovertype, NA), "char"),
         entrancedistance = use_na(testNull(x$speleothem$entrancedistance, NA), "int"),
         landusecovertype = use_na(testNull(x$speleothem$landusecovertype, NA), "char"),
         speleothemdriptype = use_na(testNull(x$speleothem$speleothemdriptype, NA), "char"),
         landusecoverpercent = use_na(testNull(x$speleothem$landusecoverpercent, NA), "int"),
         vegetationcovertype = use_na(testNull(x$speleothem$vegetationcovertype, NA), "char"),
         entitycoverthickness = use_na(testNull(x$speleothem$entitycoverthickness, NA), "int"),
         entrancedistanceunits = use_na(testNull(x$speleothem$entrancedistanceunits, NA), "char"),
         vegetationcoverpercent = use_na(testNull(x$speleothem$vegetationcoverpercent, NA), "int"))
    do.call(build_speleothem, sp)
  })
  
  sp <- new("speleothems", speleothems = speleothems)
  return(sp)
}
