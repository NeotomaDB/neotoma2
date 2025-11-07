#' @title speleothemdetails
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom dplyr bind_rows distinct mutate rename left_join
#' @importFrom purrr map
#' @param x site object
#' @returns `data.frame` with speleothem records
#' @description Obtain elements on the speleothems level
#' @examples \donttest{
#' kesang <- get_sites(sitename = "Kesang cave") %>%
#'   get_datasets() %>%
#'   filter(datasettype == "pollen") %>%
#'   get_speleothems()
#' sp <- speleothemdetails(kesang)
#' }
#' @md
#' @export
setMethod(f = "speleothemdetails",
  signature = "sites",
  definition = function(x) {
    output <- map(x@sites,
                  function(y) speleothemdetails(y)) %>%
      bind_rows() %>%
      distinct()
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
    sampset <- map(x@collunits@collunits,
                   function(y) speleothemdetails(y)) %>%
      mutate(siteid = x$siteid) %>%
      bind_rows()
    return(sampset)
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
              df <- df[, sort(names(df))]
              return(df)
            }
          })