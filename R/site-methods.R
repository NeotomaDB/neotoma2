#' @title Display a `sites` object or nested slots.
#' @name show
#' @importFrom sf st_coordinates
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @param object `sites`, `datasets`, `collunits`, `contacts` object
#' @returns NULL
#' @export
setMethod(f = "show",
          signature = "site",
          definition = function(object) {
            print(data.frame(siteid = object@siteid,
                             sitename = object@sitename,
                             lat = mean(st_coordinates(object@geography)[, 2]),
                             long = mean(st_coordinates(object@geography)[, 1]),
                             altitude = object@altitude), row.names = FALSE)
          })

#' @rdname show
#' @export
setMethod(f = "show",
          signature = "sites",
          definition = function(object) {
            map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@geography)[, 2]),
                               long = mean(st_coordinates(x@geography)[, 1]),
                               altitude = x@altitude)
              return(df)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @title sub-sub
#' @name sub-sub
#' @importFrom purrr map
#' @param x Neotoma2 nested object
#' @param i iteration in nested list
#' @description Obtain one of the elements within a `sites`,
#' `collectionunits`, `datasets`, etc... Neotoma objects.
#' @returns sliced `site` object
#' @examples \donttest{
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
              out <- map(i, function(z) {
                new("site", x@sites[[z]])
              })
              out <- new("sites", sites = out)
            }
            return(out)
          })

#' @title sub
#' @name [
#' @param x The `sites` object
#' @param i The numeric index
#' @returns sliced `site` object
#' @md
#' @export
setMethod(f = "[",
          signature = signature(x = "sites", i = "numeric"),
          definition = function(x, i) {
            new("sites", sites = x@sites[i])
          })

#' @rdname sub
#' @export
setMethod(f = "[",
          signature = signature(x = "site", i = "numeric"),
          definition = function(x, i) {
            slots <- slotNames(x)[i]
            as.data.frame(sapply(slots, function(y) slot(x, y)))
          })

#' @rdname sub
#' @export
setMethod(f = "[",
          signature = signature(x = "site", i = "character"),
          definition = function(x, i) {
            out <- as.data.frame(lapply(i, function(y) slot(x, y)))
            colnames(out) <- i
            return(out)
          })

#' @title Get a neotoma2 object's slot names
#' @name names
#' @param x A neotoma2 object.
#' @returns NULL.
#' @description Get all names for elements' slots within a `collunit` object.
#' @md
#' @export
setMethod(f = "names",
          signature = signature(x = "site"),
          definition = function(x) {
            slotNames(x)
          })

#' @rdname names
#' @export
setMethod(f = "names",
          signature = signature(x = "sites"),
          definition = function(x) {
            slotNames("site")
          })

#' @title sub-subset
#' @name sub-subset
#' @param x neotoma2 object
#' @param i iteration in neotoma2 object
#' @param value The value to be used
#' @description Obtain one of the elements within a nested neotoma2 object
#' @returns `sites` object with reassigned values
#' @md
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "sites"),
          definition = function(x, i, value) {
            siteset <- x@sites
            siteset[[i]] <- value
            out <- new("sites", sites = siteset)
            return(out)
          })

#' @title subset
#' @name subset
#' @param x A `neotoma2` object.
#' @param i The column indicator.
#' @param value The value to be used.
#' @returns `site` object with reassigned character values
#' @md
#' @export
setMethod(f = "[<-",
          signature = signature(x = "site", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(length(i))) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @rdname subset
#' @export
setMethod(f = "[<-",
          signature = signature(x = "site", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in seq_along(length(i))) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @title cash-set
#' @name cash-set
#' @param x A `neotoma2` object.
#' @param name name of the slot
#' @param value The value to be used.
#' @returns `neotoma2` object with reassigned values
#' @md
#' @export
setMethod(f = "$<-",
          signature = signature(x = "site"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @title cash
#' @name cash
#' @param x `neotoma2` object
#' @param name name of the slot
#' @description Obtain slots of a site without using at-mark
#' @returns value at chosen slot in the `site` object
#' @md
#' @export
setMethod(f = "$",
          signature = signature(x = "site"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @rdname cash
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

#' @title as.data.frame
#' @name as.data.frame
#' @importFrom sf st_coordinates
#' @param x `neotoma2` object
#' @returns `data.frame` object
#' @description Returns `neotoma2` object's data as a `data.frame`.
#' @md
#' @export
setMethod(f = "as.data.frame",
          signature = signature("site"),
          definition = function(x) {
            data.frame(siteid = x@siteid,
                       sitename = x@sitename,
                       lat = mean(st_coordinates(x@geography)[, 2]),
                       long = mean(st_coordinates(x@geography)[, 1]),
                       area = x@area,
                       notes = x@notes,
                       description = x@description,
                       elev = x@altitude)
          })

#' @rdname as.data.frame
#' @export
setMethod(f = "as.data.frame",
          signature = signature("sites"),
          definition = function(x) {
            x@sites %>% map(as.data.frame) %>% bind_rows()
          })

#' @title  as.list sites
#' @param x sites object
#' @returns `list` object with sites
#' @noRd
setMethod(f = "as.list",
          signature = signature("sites"),
          definition = function(x) {
            as.list(x@sites)
          })

#' @title length
#' @name length
#' @param x `neotoma2` object
#' @returns `int` representing length of a `neotoma2` object
#' @md
#' @export
setMethod(f = "length",
          signature = signature(x = "sites"),
          definition = function(x) {
            length(x@sites)
          })

#' @title c - Combine `neotoma2` objects
#' @name c
#' @param x `neotoma` object 1 or NULL
#' @param y `neotoma` object 2 or NULL
#' @returns concatenated and cleaned `sites` object
#' @md
#' @export
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' @rdname c
#' @export
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x = "missingORNULL", y) {
            y
          })

#' @rdname c
#' @export
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x = "missingORNULL", y) {
            y
          })

#' @rdname c
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
              data.frame(siteid = x$siteid,
                         sitename = x$sitename,
                         collunits = collunits)
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
#' @importFrom dplyr mutate group_by row_number
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
            dois <- map(datasets(x)@datasets, function(x) {
              doi <- unlist((x$doi  %>% map(testNull)))
              data.frame(datasetid = x$datasetid,
                         doi = doi)
            }) %>%
              bind_rows() %>%
              mutate(datasetid = datasetid) %>%
              full_join(ids, by = "datasetid") %>%
              select(siteid, collunitid, datasetid, doi) %>%
              group_by(siteid, collunitid, datasetid) %>%
              arrange(doi) %>%
              filter(row_number() == 1) %>%
              as.data.frame()
            return(dois)
          })

#' @rdname doi
#' @export
setMethod(f = "doi",
          signature = "site",
          definition = function(x) {
            ids <- getids(x)
            dois <- map(datasets(x)@datasets, function(x) {
              doi <- unlist((x$doi  %>% map(testNull)))
              data.frame(datasetid = x$datasetid,
                         doi = doi)
            }) %>%
              bind_rows() %>%
              full_join(ids, by = "datasetid") %>%
              select(siteid, collunitid, datasetid, doi) %>%
              group_by(siteid, collunitid, datasetid) %>%
              arrange(doi) %>%
              filter(row_number() == 1) %>%
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
            ids <- getids(x) %>%
              mutate(collunitid = as.numeric(collunitid),
                     datasetid = as.numeric(datasetid))
            sitenames <- x  %>%
              as.data.frame() %>%
              select(siteid, sitename)
            datasets <- datasets(x) %>%
              as.data.frame() %>%
              select(datasetid, datasettype, database) %>%
              mutate(datasetid = as.numeric(datasetid))
            dois <- map(datasets(get_datasets(x))@datasets, function(x) {
              doi <- unlist((x$doi  %>% map(testNull)))
              pi_list <- unlist((x$pi_list  %>% map(testNull)))
              data.frame(datasetid = x$datasetid,
                         doi = doi,
                         pi_list = paste0(sort(pi_list), collapse = "; "))
            }) %>%
              do.call(rbind, .) %>%
              mutate(datasetid = as.numeric(datasetid))
            citations <- ids %>%
              full_join(sitenames, by = "siteid") %>%
              full_join(datasets, by = "datasetid") %>%
              full_join(dois, by = "datasetid") %>%
              select(siteid, sitename, collunitid, datasetid,
                     datasettype, database, doi, pi_list) %>%
              group_by(siteid, collunitid, datasetid) %>%
              arrange(doi) %>%
              filter(row_number() == 1) %>%
              as.data.frame() %>%
              mutate(citation = sprintf(strn, pi_list, sitename,
                                        datasettype, database, doi)) %>%
              select(datasetid, citation) %>%
              mutate(datasetid = as.numeric(datasetid))
            return(citations)
          })

#' @rdname cite_data
#' @export
setMethod(f = "cite_data",
          signature = "site",
          definition = function(x) {
            strn <- paste0("%s. %s; %s dataset. ",
                           "In %s. Neotoma Paleoecology Database. doi:%s")
            ids <- getids(x) %>% 
              mutate(collunitid = as.numeric(collunitid),
                     datasetid = as.numeric(datasetid))
            sitenames <- x  %>%
              as.data.frame() %>%
              select(siteid, sitename)
            datasets <- datasets(x) %>%
              as.data.frame() %>%
              select(datasetid, datasettype, database) %>%
              mutate(datasetid = as.numeric(datasetid))
            dois <- map(datasets(get_datasets(x))@datasets, function(x) {
              doi <- unlist((x$doi  %>% map(testNull)))
              pi_list <- unlist((x$pi_list  %>% map(testNull)))
              data.frame(datasetid = x$datasetid,
                         doi = doi,
                         pi_list = paste0(sort(pi_list), collapse = "; "))
            }) %>%
              bind_rows() %>%
              mutate(datasetid = as.numeric(datasetid))
            citations <- ids %>%
              full_join(sitenames, by = "siteid") %>%
              full_join(datasets, by = "datasetid") %>%
              full_join(dois, by = "datasetid") %>%
              select(siteid, sitename, collunitid, datasetid,
                     datasettype, database, doi, pi_list) %>%
              group_by(siteid, collunitid, datasetid) %>%
              arrange(doi) %>%
              filter(row_number() == 1) %>%
              as.data.frame() %>%
              mutate(citation = sprintf(strn, pi_list, sitename,
                                        datasettype, database, doi)) %>%
              select(datasetid, citation)
            return(citations)
          })