utils::globalVariables(c("modelagetype", "isdefault", "allids", "<<-"))

#' @title chroncontrols
#' @param x sites object
#' @description Obtain chroncontrols
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

#' @title chroncontrols
#' @param x site object
#' @description Obtain elements on the samples level
#' @export
#' @import dplyr
setMethod(f = "chroncontrols",
          signature = "site",
          definition = function(x) {
            #allids <- NULL
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

