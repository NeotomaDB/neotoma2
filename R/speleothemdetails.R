#' @title speleothemdetails
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom dplyr bind_rows distinct mutate rename 
#' @importFrom dplyr left_join select arrange
#' @importFrom purrr map
#' @param x site object
#' @returns `data.frame` with speleothem records
#' @description Obtain elements on the speleothems level
#' Experimental function: API and behavior may change.
#' @examples \donttest{
#' tryCatch({
#' kesang <- get_sites(sitename = "Kesang cave") %>%
#'   get_datasets() %>%
#'   filter(datasettype == "pollen") %>%
#'   get_speleothems()
#' sp <- speleothemdetails(kesang)
#' }, error = function(e) {
#' message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @md
#' @export
setMethod(f = "speleothemdetails",
  signature = "sites",
  definition = function(x) {
    output <- map(x@sites,
                  function(y) speleothemdetails(y)) %>%
      bind_rows() %>%
      distinct() %>%
      select(.data$siteid, .data$sitename, .data$collectionunitid,
             .data$datasetid, .data$entityid, .data$entityname,
             .data$depth, .data$thickness, .data$chronologyid,
             .data$chronologyname, .data$agetype, .data$ageolder,
             .data$age, .data$ageyounger, .data$age_units, .data$sampleid,
             .data$samplename, .data$taxongroup, .data$ecologicalgroup,
             .data$taxonid, .data$variablename, .data$value, .data$units,
             # speleothem details
             .data$speleothemtype, .data$geology,
             .data$relativeage, .data$monitoring,
             .data$speleothemdriptype, .data$dripheight, .data$dripheightunits,
             .data$covertype, .data$entitycoverthickness,
             .data$entrancedistance, .data$entrancedistanceunits, 
             .data$landusecovertype, .data$landusecoverpercent,
             .data$vegetationcovertype, .data$vegetationcoverpercent) %>%
      arrange(.data$entityid, .data$taxonid, .data$depth, .data$age)
    if (nrow(output) == 0) {
      warnsite <- sprintf("No assigned speleothems. Is it a speleothem 
                           dataset? \nDid you run get_speleothems()?")
      warning(warnsite)
    }
    return(output)
  }
)

#' @rdname speleothemdetails
#' @export
setMethod(f = "speleothemdetails",
  signature = "site",
  definition = function(x) {
    map(x@collunits@collunits,
        function(y) {
          speleothemdetails(y) %>%
            bind_rows()
        })
  }
)

#' @rdname speleothemdetails
#' @export
setMethod(f = "speleothemdetails",
  signature = "collunits",
  definition = function(x) {
    df <- map(x@collunits, function(x) speleothemdetails(x)) %>%
      bind_rows()
    return(df)
  }
)

#' @rdname speleothemdetails
#' @export
setMethod(f = "speleothemdetails",
          signature = "collunit",
          definition = function(x) {
            speleothemset <- speleothems(x)
            if (nrow(speleothemset) == 0) {
              msg <- "No assigned speleothems. Is it a speleothem dataset? \n
                                  Did you run get_speleothems()?"
              warnsite <- sprintf(msg)
              warning(warnsite)
              return(data.frame())
            } else {
              x <- get_downloads(speleothemset$datasetid)
              sp_samples <- samples(x) %>% distinct()
              # join speleothemset and sp_samples on datasetid
              df <- speleothemset %>%
                left_join(sp_samples,
                          by = "datasetid") %>%
                distinct()
              return(df)
            }
          })