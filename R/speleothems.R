#' @title speleothems
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
setMethod(f = "speleothems",
          signature = "sites",
          definition = function(x) {
            output <- purrr::map(x@sites, function(y) speleothems(y)) %>%
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

#' @title speleothems
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
#' @importFrom dplyr bind_rows distinct select filter left_join rename
setMethod(f = "speleothems",
          signature = "site",
          definition = function(x) {
            allids <- getids(x) %>% 
              dplyr::distinct() %>%
              dplyr::select(siteid, datasetid)
            dsids <- as.data.frame(datasets(x)) %>%
              dplyr::filter(datasettype == "speleothem")
            dsids <- dsids$datasetid
            allids <- allids %>%
              dplyr::filter(datasetid %in% dsids)
            siteinfo <- as.data.frame(x) %>%
              dplyr::distinct() %>%
              dplyr::left_join(allids, by = "siteid")
            sampset <- purrr::map(x@collunits@collunits,
                                  function(y) speleothems(y)) %>%
              dplyr::bind_rows() %>%
              dplyr::left_join(siteinfo, by = "datasetid") %>%
              dplyr::rename(sitenotes = notes)
            return(sampset)
          }
)

#' @title Get speleothems from a collectionunit or set of collection units:
#' @param x collunits object
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @returns `data.frame` with sample records
#' @description Obtain elements from collunits
setMethod(f = "speleothems",
          signature = "collunits",
          definition = function(x) {
            df <- purrr::map(x@collunits, function(x) speleothems(x)) %>%
              dplyr::bind_rows()
            return(df)
          }
)

#' @title speleothems
#' @param x collunit object
#' @description Obtain elements from collunit
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate
#' @returns `data.frame` with sample records
#' @export
setMethod(f = "speleothems",
          signature = "collunit",
          definition = function(x) {
            dsids <- as.data.frame(datasets(x)) %>%
              dplyr::filter(datasettype == "speleothem") %>%
              dplyr::mutate(collectionunitid = x@collectionunitid)
            
            if (length(x@speleothems@speleothems) == 0) {
              warnsite <- sprintf("No assigned speleothems. Is it a speleothems dataset? Did you run `get_speleothems()`?")
              warning(warnsite)
              return(data.frame())
            } else {
            speleothemset <- purrr::map(x@speleothems@speleothems,
                                        function(y) {
                                          y <- as.data.frame(y)
                                          if (!is.null(y) && nrow(y) > 0) {
                                            df <- data.frame(collunitid = y$collectionunitid,
                                                             entityid = y$entityid,
                                                             entityname = y$entityname,
                                                             speleothemtype = y$speleothemtype,
                                                             speleothemdriptype = y$speleothemdriptype,
                                                             dripheight = y$dripheight,
                                                             dripheightunits = y$dripheightunits,
                                                             monitoring = y$monitoring,
                                                             geology = y$geology,
                                                             relativeage = y$relativeage,
                                                             covertype = y$entitycovertype,
                                                             entitycoverthickness = y$entitycoverthickness,
                                                             entrancedistance = y$entrancedistance,
                                                             entrancedistanceunits = y$entrancedistanceunits,
                                                             landusecovertype = y$landusecovertype,
                                                             landusecoverpercent = y$landusecoverpercent,
                                                             vegetationcovertype = y$vegetationcovertype,
                                                             vegetationcoverpercent = y$vegetationcoverpercent) %>%
                                              dplyr::left_join(dsids %>% dplyr::select(collunitid=collectionunitid,
                                                                                       datasetid=datasetid), by = "collunitid")
                                          } else {
                                            df <- data.frame()
                                          }
                                          return(df)
                                        }) %>%
              dplyr::bind_rows()
            speleothemset  <- speleothemset %>%
              distinct()
            return(speleothemset)
            }
          })