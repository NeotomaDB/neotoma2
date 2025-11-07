#' @title speleothems
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom dplyr bind_rows distinct select filter left_join rename
#' @importFrom purrr map
#' @param x sites object
#' @returns `data.frame` with sample records
#' @description Obtain all speleothems within a sites object
#' @examples {
#' ds <- get_datasets(37302)
#' sp <- speleothems(ds)
#' }
#' @md
#' @export
setMethod(f = "speleothems",
  signature = "sites",
  definition = function(x) {
    output <- map(x@sites,
                  function(y) speleothems(y)) %>%
      bind_rows() %>%
      distinct()
    if (nrow(output) == 0) {
      msg <- "No assigned speleothems. Is it a speleothem dataset? \n
                          Did you run get_speleothems()?"
      warnsite <- sprintf(msg)
      warning(warnsite)
    }
    return(output)
  }
)

#' @rdname speleothems
#' @export
setMethod(f = "speleothems",
  signature = "site",
  definition = function(x) {
    allids <- getids(x) %>%
      distinct() %>%
      select(siteid, datasetid)
    dsids <- as.data.frame(datasets(x)) %>%
      filter(datasettype == "speleothem")
    dsids <- dsids$datasetid
    allids <- allids %>%
      filter(datasetid %in% dsids)
    siteinfo <- as.data.frame(x) %>%
      distinct() %>%
      left_join(allids, by = "siteid")
    sampset <- map(x@collunits@collunits,
                   function(y) speleothems(y)) %>%
      bind_rows() %>%
      left_join(siteinfo, by = "datasetid") %>%
      rename(sitenotes = notes)
    return(sampset)
  }
)

#' @rdname speleothems
#' @export
setMethod(f = "speleothems",
  signature = "collunits",
  definition = function(x) {
    df <- map(x@collunits, function(x) speleothems(x)) %>%
      bind_rows()
    return(df)
  }
)

#' @rdname speleothems
#' @export
setMethod(f = "speleothems",
  signature = "collunit",
  definition = function(x) {
    dsids <- as.data.frame(datasets(x)) %>%
      filter(datasettype == "speleothem") %>%
      mutate(collectionunitid = x@collectionunitid)
    if (length(x@speleothems@speleothems) == 0) {
      warning(sprintf("No assigned speleothems. Is it a speleothems dataset?
              Did you run `get_speleothems()`?"))
      return(data.frame())
    } else {
      speleothemset <- map(x@speleothems@speleothems,
                           function(y) {
                             y <- as.data.frame(y)
                             if (!is.null(y) && nrow(y) > 0) {
                               df <-
                                 data.frame(collunitid = y$collectionunitid,
                                            entityid = y$entityid,
                                            entityname = y$entityname,
                                            speleothemtype = y$speleothemtype,
                                            speleothemdriptype =
                                            y$speleothemdriptype,
                                            dripheight = y$dripheight,
                                            dripheightunits =
                                            y$dripheightunits,
                                            monitoring = y$monitoring,
                                            geology = y$geology,
                                            relativeage = y$relativeage,
                                            covertype = y$entitycovertype,
                                            entitycoverthickness =
                                            y$entitycoverthickness,
                                            entrancedistance =
                                            y$entrancedistance,
                                            entrancedistanceunits =
                                            y$entrancedistanceunits,
                                            landusecovertype =
                                            y$landusecovertype,
                                            landusecoverpercent =
                                            y$landusecoverpercent,
                                            vegetationcovertype =
                                            y$vegetationcovertype,
                                            vegetationcoverpercent =
                                            y$vegetationcoverpercent) %>%
                                 left_join(dsids %>%
                                             select(collunitid =
                                                      dsids$collectionunitid,
                                                    datasetid = datasetid),
                                           by = "collunitid")
                             } else {
                               df <- data.frame()
                             }
                             return(df)
                           }) %>%
        bind_rows()
      speleothemset  <- speleothemset %>%
        distinct()
      return(speleothemset)
    }
  }
)