#' @md
#' @title Build a `speleothem` from the Neotoma API response.
#' @param args A list returned from the Neotoma API `data` section.
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @import sf
#' @returns A simple `speleothem` object
#' @keywords internal
#' @noRd
build_speleothem <- function(...) {
  args <- list(...)
  assertthat::assert_that(is.list(args),
                          msg = "Parsed object must be a list.")
  
 speleothem <- set_speleothem(entityid = use_na(testNull(args$entityid, NA), "int"),
                        entityname = use_na(testNull(args$entityname, NA), "char"),
                        siteid = use_na(testNull(args$siteid, NA), "int"),
                        collectionunitid = use_na(testNull(args$collectionunitid, NA), "int"),
                        datasetid = use_na(testNull(args$datasetid, NA), "int"),
                        dripheight = use_na(testNull(args$dripheight, NA), "int"),
                        dripheightunits = use_na(testNull(args$dripheightunits, NA), "char"),
                        monitoring = use_na(testNull(args$monitoring, NA), "logic"),
                        relativeage = use_na(testNull(args$relativeage, NA), "char"),
                        speleothemtype = use_na(testNull(args$speleothemtype, NA), "char"),
                        entitycovertype = use_na(testNull(args$entitycovertype, NA), "char"),
                        entrancedistance = use_na(testNull(args$entrancedistance, NA), "int"),
                        landusecovertype = use_na(testNull(args$landusecovertype, NA), "char"),
                        speleothemdriptype = use_na(testNull(args$speleothemdriptype, NA), "char"),
                        landusecoverpercent = use_na(testNull(args$landusecoverpercent, NA), "int"),
                        vegetationcovertype = use_na(testNull(args$vegetationcovertype, NA), "char"),
                        entitycoverthickness = use_na(testNull(args$entitycoverthickness, NA), "int"),
                        entrancedistanceunits = use_na(testNull(args$entrancedistanceunits, NA), "char"),
                        vegetationcoverpercent = use_na(testNull(args$vegetationcoverpercent, NA), "int"))
  return(speleothem)
}
