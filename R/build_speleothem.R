#' @title Build a `speleothem` from the Neotoma API response.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom assertthat assert_that
#' @param args A list returned from the Neotoma API `data` section.
#' @returns A simple `speleothem` object
#' @noRd
build_speleothem <- function(...) {
  args <- list(...)
  args <- cleanNULL(args)
  assert_that(is.list(args), msg = "Parsed object must be a list.")
  # A speleothem coming from the API must carry a real entityid. When it does
  # not, the element is malformed/empty, so we drop it rather than fabricate a
  # phantom speleothem with a random id (see set_speleothem()). The caller
  # null-filters before building the `speleothems` container.
  if (is.null(args$entityid)) {
    return(NULL)
  }
  speleothem <- set_speleothem(
                  entityid = use_na(args$entityid, "int"),
                  entityname = use_na(args$entityname, "char"),
                  siteid = use_na(args$siteid, "int"),
                  collectionunitid = use_na(args$collectionunitid, "int"),
                  dripheight = use_na(args$dripheight, "int"),
                  dripheightunits = use_na(args$dripheightunits, "char"),
                  monitoring = use_na(args$monitoring, "logic"),
                  geology = use_na(args$geology, "char"),
                  rockage = use_na(args$rockage, "char"),
                  speleothemtype = use_na(args$speleothemtype, "char"),
                  entitycovertype = use_na(args$entitycovertype, "char"),
                  entrancedistance = use_na(args$entrancedistance, "int"),
                  landusecovertype = use_na(args$landusecovertype, "char"),
                  speleothemdriptype = use_na(args$speleothemdriptype, "char"),
                  landusecoverpercent = use_na(args$landusecoverpercent, "int"),
                  vegetationcovertype = use_na(args$vegetationcovertype,
                                               "char"),
                  entitycoverthickness = use_na(args$entitycoverthickness,
                                                "int"),
                  entrancedistanceunits = use_na(args$entrancedistanceunits,
                                                 "char"),
                  vegetationcoverpercent = use_na(args$vegetationcoverpercent,
                                                  "int"))
  return(speleothem)
}