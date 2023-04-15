utils::globalVariables(c("modelagetype", "isdefault", "allids"))

#' @title Recover information about the chron controls for a collectionunit.
#' @description For all sites that includes collection units with chronologies
#' return the chronological controls that are used in building the chronology.
#' @param x sites object
#' @returns data.frame with chronological controls
#' @export
#' @import dplyr
setMethod(f = "chroncontrols",
          signature = "sites",
          definition = function(x) {
            output <- purrr::map(x@sites, function(y) chroncontrols(y)) %>%
              dplyr::bind_rows()
            return(output)
          }
)

#' @title Recover information about the chron controls for a collectionunit.
#' @description For a site that includes collection units with chronologies
#' return the chronological controls that are used in building the chronology.
#' @param x site object
#' @export
#' @returns data.frame with chronological controls
#' @import dplyr
setMethod(f = "chroncontrols",
          signature = "site",
          definition = function(x) {

            siteid <- as.data.frame(x)$siteid
            chrons <- chronologies(x)
            chronset <- purrr::map(chrons@chronologies, function(y) {
              data.frame(chronologyid = y@chronologyid,
                         y@chroncontrols)
            }) %>%
              dplyr::bind_rows() %>%
              dplyr::mutate(siteid = siteid)

            chronset <- chronset %>%
              select(siteid, everything())

            return(chronset)
          }
)