#' @title Recover information about the chron controls for a collectionunit.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map
#' @description For all sites that includes collection units with chronologies
#' return the chronological controls that are used in building the chronology.
#' @param x sites object
#' @returns data.frame with chronological controls
#' @export
setMethod(f = "chroncontrols",
  signature = "sites",
  definition = function(x) {
    output <- map(x@sites, function(y) chroncontrols(y)) %>%
      bind_rows()
    return(output)
  }
)

#' @title Recover information about the chron controls for a collectionunit.
#' @description For a site that includes collection units with chronologies
#' return the chronological controls that are used in building the chronology.
#' @importFrom dplyr bind_rows select mutate everything
#' @importFrom purrr map
#' @param x site object
#' @returns data.frame with chronological controls
#' @export
setMethod(f = "chroncontrols",
  signature = "site",
  definition = function(x) {
    siteid <- as.data.frame(x)$siteid
    chrons <- chronologies(x)
    chronset <- map(chrons@chronologies, function(y) {
      data.frame(chronologyid = y@chronologyid,
                 y@chroncontrols)
    }) %>%
      bind_rows() %>%
      mutate(siteid = siteid)
    chronset <- chronset %>%
      select(siteid, everything())
    return(chronset)
  }
)