utils::globalVariables(c("siteid", "collunitid", "sitename", "datasetid",
  "datasettype", "database", "pi_list", ".", "citation",
  "notes.x", "notes.y", "allids", "element", "taxonid", "symmetry",
  "taxongroup", "elementtype", "variablename", "ecologicalgroup", "element",
  "age", "value", "counter", "prop"))

#' @title Show a site object as a dataframe
#' @description Convert a Neotoma package site object into a data.frame()
#' returning the siteid, sitename, latitude, longitude and altitude of the site.
#' @param object site object
#' @returns NULL - side effect for printing a `data.frame` object
#' @export
setMethod(f = "show",
          signature = "site",
          definition = function(object) {
            print(data.frame(siteid = as.character(object@siteid),
                             sitename = object@sitename,
                             lat = mean(st_coordinates(object@geography)[, 2]),
                             long = mean(st_coordinates(object@geography)[, 1]),
                             altitude = object@altitude), row.names = FALSE)
          })

#' @title Show sites objects as a dataframe
#' @description Return a set of site objects as a single data.frame().
#' @param object sites object
#' @returns NULL - side effect for printing a `data.frame` object
#' @export
setMethod(f = "show",
          signature = "sites",
          definition = function(object) {
            map(object@sites, function(x) {
              df <- data.frame(siteid = as.character(x@siteid),
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@geography)[, 2]),
                               long = mean(st_coordinates(x@geography)[, 1]),
                               altitude = x@altitude)
              return(df)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @title  Slicer
#' @param x sites object
#' @param i iteration in sites list
#' @description Obtain one of the elements within a sites list
#' @returns sliced `site` object
#' @examples {
#' some_site <- get_sites(sitename = "Site%", limit=3)
#' some_site[[2]]
#' }
#' @export
setMethod(f = "[[",
          signature = signature(x = "sites", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("site", x@sites[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("site", x@sites[[z]])
              })
              out <- new("sites", sites = out)
            }
            return(out)
          })

#' @title Get or remove sites by numeric index
#' @param x The sites object
#' @param i The numeric index
#' @returns sliced `site` object
#' @export
setMethod(f = "[",
          signature = signature(x = "sites", i = "numeric"),
          definition = function(x, i) {
            new("sites", sites = x@sites[i])
          })

#' @title Get site field by numeric index
#' @param x The site object
#' @param i The column indicator
#' @returns sliced `site` object
setMethod(f = "[",
          signature = signature(x = "site", i = "numeric"),
          definition = function(x, i) {
            slots <- slotNames(x)[i]
            as.data.frame(sapply(slots, function(y) slot(x, y)))
          })

#' @title Get site field by character index
#' @param x The site object
#' @param i The column indicator
#' @returns sliced `site` object
setMethod(f = "[",
          signature = signature(x = "site", i = "character"),
          definition = function(x, i) {
            out <- as.data.frame(lapply(i, function(y) slot(x, y)))
            colnames(out) <- i
            return(out)
          })

#' @title Get slot names
#' @param x A site object.
#' @description Get all names for named elements within a `site` object.
#' @returns names of the slots of a `site` object
#' @export
setMethod(f = "names",
          signature = signature(x = "site"),
          definition = function(x) {
            slotNames(x)
          })

#' @title  Insert site
#' @param x sites object
#' @param i iteration in sites list
#' @param value The value to be used
#' @description Obtain one of the elements within a sites list
#' @returns `sites` object with reassigned values
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "sites"),
          definition = function(x, i, value) {
            siteset <- x@sites
            siteset[[i]] <- value
            out <- new("sites", sites = siteset)
            return(out)
          })

#' @title Assign site field by numeric index
#' @param x The site object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `site` object with reassigned character values
setMethod(f = "[<-",
          signature = signature(x = "site", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(length(i))) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @title Assign site field by numeric index
#' @param x The site object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `sites` object with reassigned numeric values
setMethod(f = "[<-",
          signature = signature(x = "site", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in seq_along(length(i))) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title Assign site field by numeric index
#' @param x The site object.
#' @param name name of the slot
#' @param value The value to be used.
#' @returns `site` object with reassigned values
setMethod(f = "$<-",
          signature = signature(x = "site"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @title $
#' @param x site object
#' @param name name of the slot
#' @description Obtain slots of a site without using at-mark
#' @returns value at chosen slot in the `site` object
#' @export
setMethod(f = "$",
          signature = signature(x = "site"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for sites
#' @param x sites object
#' @param name name of the slot
#' @description Obtain slots of a site without using at-mark
#' @returns value at chosen slot in the `site` object
#' @export
setMethod(f = "$",
          signature = signature(x = "sites"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @title  as.data.frame site
#' @param x site object
#' @description show as dataframe as prep to save as csv
#' @returns `data.frame` object with site metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature("site"),
          definition = function(x) {
            data.frame(siteid = as.character(x@siteid),
                       sitename = x@sitename,
                       lat = mean(st_coordinates(x@geography)[, 2]),
                       long = mean(st_coordinates(x@geography)[, 1]),
                       area = x@area,
                       notes = x@notes,
                       description = x@description,
                       elev = x@altitude)
          })

#' @title  as.data.frame sites
#' @param x sites object
#' @description shows object as data.frame
#' @returns `data.frame` object with sites metadata
#' @export
setMethod(f = "as.data.frame",
          signature = signature("sites"),
          definition = function(x) {
            x@sites %>% map(as.data.frame) %>% bind_rows()
          })

#' @title  as.list sites
#' @param x sites object
#' @description show as dataframe as prep to save as csv
#' @returns `list` object with sites metadata
#' @export
setMethod(f = "as.list",
          signature = signature("sites"),
          definition = function(x) {
            as.list(x@sites)
          })

#' @title Length Method Sites
#' @export
#' @param x sites object
#' @returns `int` with the length of sites object
setMethod(f = "length",
          signature = signature(x = "sites"),
          definition = function(x) {
            length(x@sites)
          })

#' @title c Method - Combine sites objects
#' @param x sites object 1
#' @param y sites object 2
#' @returns concatenated and cleaned `sites` object
#' @export
setMethod(f = "c",
          signature = signature(x = "sites"),
          definition = function(x, y) {
            if (is(y, "sites")) {
              out <- new("sites",
                         sites = unlist(c(x@sites,
                                          y@sites),
                                        recursive = FALSE))
              out <- clean(out)
            } else if (is(y, "site")) {
              siteset <- c(x@sites, y)
              out <- new("sites", sites = siteset)
              out <- clean(out)
            }
            return(out)
          })

#' @title write CSV
#' @param x A sites object
#' @param ... Other options to pass to \code{write.csv()}.
#' @importFrom utils write.csv
#' @returns NULL side effect from saving a csv file
#' @export
setMethod(f = "write.csv",
          signature = "sites",
          definition = function(x, ...) {
            df1 <- as.data.frame(x)
            write.csv(df1, ...)
          })

#' @title Return the latitude and longitude of sites
#' @param obj A sites object
#' @param ... Additional parameters associated with the call.
#' @returns `data.frame` object with site coordinates.
#' @export
setMethod(f = "coordinates",
          signature = "sites",
          definition = function(obj, ...) {
            coords <- as.data.frame(obj)[, c("long", "lat")]
            return(coords)
          })

#' @title Plot site coordinates using a basic plot.
#' @param x sites object
#' @param y ANY
#' @param ... Additional parameters associated with the call.
#' @returns `plot` object with site coordinates.
#' @export
setMethod(f = "plot",
          signature = "sites",
          definition = function(x, ...) {
            coords <- as.data.frame(x)[, c("long", "lat")]
            plot(coords, ...)
          })

#' @title Summary of objects within a sites object.
#' @param object sites object
#' @param ... additional properties passed to \code{summary}
#' @description This function summarizes a sites object, from \code{site} level 
#' and returns a \code{data.frame} that contains the site ID, sitename,
#' collectionunit ID, count of chronologies, count of datasets
#' and types of datasets within the site.
#' @importFrom dplyr bind_rows mutate select
#' @returns `data.frame` object with site summary information
#' @export
setMethod(f = "summary",
          signature = "sites",
          definition = function(object, ...) {

            datasettype <- lapply(object@sites, function(x) {

              collunits <- length(x@collunits@collunits)

              if (length(x) > 0) {
                collunits <- lapply(x@collunits@collunits,
                                       function(y) {
                                         chrons <- length(y@chronologies)
                                         datasets <- length(y@datasets)
                                         if (datasets > 0) {
                                           types <- sapply(y@datasets@datasets,
                                                           function(r) {
                                                             r@datasettype
                                                           }) %>%
                                             paste0(collapse = ",")
                                         } else {
                                           types <- NA
                                         }
                                         data.frame(collectionunit = y@handle,
                                                    chronologies = chrons,
                                                    datasets = datasets,
                                                    types = types)
                                         }) %>%
                                         bind_rows()
              } else {
                collunits <- data.frame(collectionunit = NA,
                                        chronologies = 0,
                                        datasets = 0,
                                        types = NA)
              }

              data.frame(siteid = as.character(x$siteid), sitename = x$sitename,
                         collunits)
              }
            ) %>%
            bind_rows()

            collunits <- lapply(object@sites, function(x) {
              datasets <- sapply(x@collunits@collunits,
                                 function(y) length(y@datasets))

              chronologies <- sapply(x@collunits@collunits,
                                     function(y) length(y@chronologies))

              return(data.frame(siteid = as.character(x$siteid),
                                collunits = length(x@collunits),
                                datasets = datasets))
            }) %>%
            bind_rows()
            return(datasettype)
          })

#' @title Obtain dataset DOIs from records.
#' @description Given complete dataset objects in Neotoma (must have used
#' \code{get_datasets()} or \code{get_downloads()}), return the dataset
#' DOI for the record.
#' @param x a Neotoma2 \code{site} object
#' @importFrom purrr map
#' @importFrom dplyr bind_rows full_join select arrange filter
#' @returns `data.frame` object with DOIs information.
#' @examples {
#' ds <- get_datasets(1)
#' doi(ds)
#' }
#' @export
setMethod(f = "doi",
          signature = "sites",
          definition = function(x) {
            ids <- getids(x)
            dois <- purrr::map(datasets(x)@datasets, function(x) {
              doi <- unlist((x$doi  %>% purrr::map(testNull)))
              data.frame(datasetid = x$datasetid,
                         doi = doi)
            }) %>%
            dplyr::bind_rows() %>%
            dplyr::mutate(datasetid = as.character(datasetid)) %>%
            dplyr::full_join(ids, by = "datasetid") %>%
            dplyr::select(siteid, collunitid, datasetid, doi) %>%
            dplyr::group_by(siteid, collunitid, datasetid) %>%
            dplyr::arrange(doi) %>%
            dplyr::filter(dplyr::row_number() == 1) %>%
            as.data.frame()
            return(dois)
          })

#' @title Obtain dataset DOIs from records.
#' @description Given complete dataset objects in Neotoma (must have used
#' \code{get_datasets()} or \code{get_downloads()}), return the dataset
#' DOI for the record.
#' @param x a Neotoma2 \code{site} object
#' @importFrom purrr map
#' @importFrom dplyr bind_rows full_join select arrange filter
#' @returns `data.frame` object with DOIs information.
#' @examples {
#' ds <- get_datasets(1)
#' doi(ds)
#' }
#' @export
setMethod(f = "doi",
          signature = "site",
          definition = function(x) {
            ids <- getids(x)
            dois <- purrr::map(datasets(x)@datasets, function(x) {
              doi <- unlist((x$doi  %>% purrr::map(testNull)))
              data.frame(datasetid = x$datasetid,
                         doi = doi)
            }) %>%
            dplyr::bind_rows() %>%
            dplyr::full_join(ids, by = "datasetid") %>%
            dplyr::select(siteid, collunitid, datasetid, doi) %>%
            dplyr::group_by(siteid, collunitid, datasetid) %>%
            dplyr::arrange(doi) %>%
            dplyr::filter(dplyr::row_number() == 1) %>%
            as.data.frame()
            return(dois)
          })

#' @title Obtain data citations from multiple records.
#' @description Given complete dataset objects in Neotoma (must have used
#' \code{get_datasets()} or \code{get_downloads()}), return a formatted
#' citation for the record, including the dataset DOI.
#' @param x sites object
#' @importFrom purrr map
#' @importFrom dplyr bind_rows full_join select arrange filter
#' @returns `data.frame` object with citation information.
#' @examples {
#' ds <- get_datasets(1)
#' cite_data(ds)
#' }
#' @export
setMethod(f = "cite_data",
          signature = "sites",
          definition = function(x) {
            strn <- paste0("%s. %s; %s dataset. ",
                           "In %s. Neotoma Paleoecology Database. doi:%s")
            ids <- getids(x) %>% mutate(
              collunitid = as.numeric(collunitid),
              datasetid = as.numeric(datasetid))
            
            sitenames <- x  %>%
              as.data.frame() %>%
              dplyr::select(siteid, sitename)
            datasets <- datasets(x) %>%
              as.data.frame() %>%
              dplyr::select(datasetid, datasettype, database)%>%
              mutate(datasetid = as.numeric(datasetid))

            dois <- purrr::map(datasets(get_datasets(x))@datasets, function(x) {
              doi <- unlist((x$doi  %>% purrr::map(testNull)))
              pi_list <- unlist((x$pi_list  %>% purrr::map(testNull)))
              data.frame(datasetid = x$datasetid,
                         doi = doi,
                         pi_list = paste0(sort(pi_list), collapse = "; "))
            }) %>%
            do.call(rbind, .) %>%
              mutate(datasetid = as.numeric(datasetid))

            citations <- ids %>%
              dplyr::full_join(sitenames, by = "siteid") %>%
              dplyr::full_join(datasets, by = "datasetid") %>%
              dplyr::full_join(dois, by = "datasetid") %>%
              dplyr::select(siteid, sitename,
                            collunitid, datasetid,
                            datasettype, database, doi, pi_list) %>%
              dplyr::group_by(siteid, collunitid, datasetid) %>%
              dplyr::arrange(doi) %>%
              dplyr::filter(dplyr::row_number() == 1) %>%
              as.data.frame() %>%
              dplyr::mutate(citation = sprintf(strn,
                            pi_list, sitename, datasettype, database, doi)) %>%
              dplyr::select(datasetid, citation) %>%
              mutate(datasetid = as.numeric(datasetid))

            return(citations)
          })


#' @title Obtain data citations from a single record.
#' @description Given complete dataset objects in Neotoma (must have used
#' \code{get_datasets()} or \code{get_downloads()}), return a formatted
#' citation for the record, including the dataset DOI.
#' @param x sites object
#' @importFrom purrr map
#' @importFrom dplyr bind_rows full_join select arrange filter
#' @returns `data.frame` object with citation information.
#' @examples 
#' \donttest{
#' ds <- get_datasets(1)
#' cite_data(ds)
#' }
#' @export
setMethod(f = "cite_data",
          signature = "site",
          definition = function(x) {
            strn <- paste0("%s. %s; %s dataset. ",
                           "In %s. Neotoma Paleoecology Database. doi:%s")
            ids <- getids(x) %>% mutate(
              collunitid = as.numeric(collunitid),
              datasetid = as.numeric(datasetid))
            
            sitenames <- x  %>%
              as.data.frame() %>%
              dplyr::select(siteid, sitename) 
            datasets <- datasets(x) %>%
              as.data.frame() %>%
              dplyr::select(datasetid, datasettype, database) %>%
              mutate(datasetid = as.numeric(datasetid))

            dois <- purrr::map(datasets(get_datasets(x))@datasets, function(x) {
              doi <- unlist((x$doi  %>% purrr::map(testNull)))
              pi_list <- unlist((x$pi_list  %>% purrr::map(testNull)))
              data.frame(datasetid = x$datasetid,
                         doi = doi,
                         pi_list = paste0(sort(pi_list), collapse = "; "))
            }) %>%
            dplyr::bind_rows() %>%
              mutate(datasetid = as.numeric(datasetid))

            citations <- ids %>%
            dplyr::full_join(sitenames, by = "siteid") %>%
            dplyr::full_join(datasets, by = "datasetid") %>%
            dplyr::full_join(dois, by = "datasetid") %>%
            dplyr::select(siteid, sitename,
                          collunitid, datasetid,
                          datasettype, database, doi, pi_list) %>%
            dplyr::group_by(siteid, collunitid, datasetid) %>%
            dplyr::arrange(doi) %>%
            dplyr::filter(dplyr::row_number() == 1) %>%
            as.data.frame() %>%
            dplyr::mutate(citation = sprintf(strn,
              pi_list, sitename, datasettype, database, doi)) %>%
            dplyr::select(datasetid, citation)

            return(citations)
          })
