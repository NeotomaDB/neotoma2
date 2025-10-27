#' @title speleothemdetails
#' @param x sites object
#' @description Obtain all speleothems within a sites object
#' @examples {
#' ds <- get_datasets(37302)
#' sp <- speleothems(ds)
#' }
#' @export
#' @importFrom dplyr bind_rows distinct
#' @importFrom purrr map
#' @returns `data.frame` with sample records
setMethod(f = "speleothemdetails",
          signature = "sites",
          definition = function(x) {
            output <- purrr::map(x@sites, function(y) speleothemdetails(y)) %>%
              dplyr::bind_rows() %>%
              dplyr::distinct()
            if(nrow(output) == 0){
              warnsite <- sprintf("No assigned speleothems. Is it a speleothem dataset? \n
                                  Did you run get_speleothems()?")
              warning(warnsite)
            }
            return(output)
          }
)

#' @title speleothemdetails
#' @param x site object
#' @description Obtain elements on the speleothems level
#' @examples \donttest{
#' kesang <- get_sites(sitename = "Kesang cave") %>%
#'   get_datasets() %>%
#'   filter(datasettype == "pollen") %>%
#'   get_speleothems()
#' sp <- speleothems(kesang)
#' }
#' @export
#' @returns `data.frame` with speleothem records
#' @importFrom dplyr bind_rows distinct mutate rename
setMethod(f = "speleothemdetails",
          signature = "site",
          definition = function(x) {
            sampset <- purrr::map(x@collunits@collunits,
                                  function(y) speleothemdetails(y) %>%
              dplyr::mutate(siteid = x$siteid)) %>%
              dplyr::bind_rows()
            return(sampset)
          }
)

#' @title speleothemdetails
#' @param x collunits object
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @returns `data.frame` with sample records
#' @description Obtain elements from collunits
setMethod(f = "speleothemdetails",
          signature = "collunits",
          definition = function(x) {
            df <- purrr::map(x@collunits, function(x) speleothemdetails(x)) %>%
              dplyr::bind_rows()
            return(df)
          }
)

#' @title speleothemdetails
#' @param x collunit object
#' @description Obtain elements from collunit
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate
#' @returns `data.frame` with sample and speleothemdetails records
#' @export
setMethod(f = "speleothemdetails",
          signature = "collunit",
          definition = function(x) {
            speleothemset <- speleothems(x)
            x <- get_downloads(speleothemset$datasetid)
            sp_samples <- samples(x) %>% distinct()
            # join speleothemset and sp_samples on datasetid
            df <- speleothemset %>%
              dplyr::left_join(sp_samples,
                               by = "datasetid") %>%
              dplyr::distinct()
            df <- df[ , sort(names(df))]
            return(df)
          })