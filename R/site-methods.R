setClassUnion("missingOrNULL", c("missing", "NULL"))
setClassUnion("sitesOrsite", c("site", "sites"))

#' @title Display a `sites` object or nested slots.
#' @name show
#' @importFrom sf st_coordinates
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @param object `sites`, `datasets`, `collunits`, `contacts` object
#' @returns NULL
#' @aliases show,site-method
#' @importMethodsFrom methods show
setMethod(f = "show",
          signature = "site",
          definition = function(object) {
            print(data.frame(siteid = object@siteid,
                             sitename = object@sitename,
                             lat = mean(st_coordinates(object@geography)[, 2]),
                             long = mean(st_coordinates(object@geography)[, 1]),
                             altitude = object@altitude), row.names = FALSE) %>%
              unique()
          })

#' @aliases show,sites-method
#' @rdname show
setMethod(f = "show",
          signature = "sites",
          definition = function(object) {
            map(object@sites, function(x) {
              data.frame(
                siteid = x@siteid,
                sitename = x@sitename,
                lat = mean(st_coordinates(x@geography)[, 2]),
                long = mean(st_coordinates(x@geography)[, 1]),
                altitude = x@altitude
              )
            }) %>%
              bind_rows() %>%
              unique() %>%
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
#' tryCatch({
#'   some_site <- get_sites(sitename = "Site%", limit=3)
#'   some_site[[2]]
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @aliases [[,sites,numeric-method
#' @exportMethod [[
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
#' @aliases  [,sites,numeric-method
#' @exportMethod [
setMethod(f = "[",
          signature = signature(x = "sites", i = "numeric"),
          definition = function(x, i) {
            new("sites", sites = x@sites[i])
          })

#' @aliases [,site,numeric-method
#' @rdname sub
setMethod(f = "[",
          signature = signature(x = "site", i = "numeric"),
          definition = function(x, i) {
            slots <- slotNames(x)[i]
            as.data.frame(sapply(slots, function(y) slot(x, y)))
          })

#' @aliases [,site,character-method
#' @rdname sub
setMethod(f = "[",
          signature = signature(x = "site", i = "character"),
          definition = function(x, i) {
            out <- as.data.frame(lapply(i, function(y) slot(x, y)))
            colnames(out) <- i
            return(out)
          })

#' @title Get a `neotoma2` object's slot names
#' @name names
#' @param x A `neotoma2` object.
#' @returns NULL.
#' @description Get all names for elements' slots within a `collunit` object.
#' @md
#' @aliases names,site-method
#' @exportMethod names
setMethod(f = "names",
          signature = signature(x = "site"),
          definition = function(x) {
            slotNames(x)
          })

#' @aliases names,sites-method
#' @rdname names
setMethod(f = "names",
          signature = signature(x = "sites"),
          definition = function(x) {
            slotNames("site")
          })

#' @title sub-subset
#' @name sub-subset
#' @param x `neotoma2` object
#' @param i iteration in `neotoma2` object
#' @param value The value to be used
#' @description Obtain one of the elements within a nested `neotoma2` object
#' @returns `sites` object with reassigned values
#' @md
#' @aliases [[<-,sites-method
#' @exportMethod [[<-
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
#' @aliases [<-,site,character-method
#' @exportMethod [<-
setMethod(f = "[<-",
          signature = signature(x = "site", i = "character"),
          definition = function(x, i, value) {
            for (idx in seq_along(length(i))) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @aliases [<-,site,numeric-method
#' @rdname subset
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
#' @aliases $<-,site-method
#' @exportMethod $<-
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
#' @description Obtain slots of a `neotoma2` without using at-mark
#' @returns value at chosen slot in the `site` object
#' @md
#' @aliases $,site-method
#' @exportMethod $
setMethod(f = "$",
          signature = signature(x = "site"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @aliases $,sites-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "sites"),
          definition = function(x, name) {
            vals <- lapply(x@sites, function(y) slot(y, name))
            out <- unlist(vals, recursive = FALSE)
            return(out)
          })

#' @title as.data.frame
#' @name as.data.frame
#' @importFrom sf st_coordinates
#' @importFrom dplyr bind_cols
#' @param x `neotoma2` object
#' @returns `data.frame` object
#' @description Returns `neotoma2` object's data as a `data.frame`.
#' @md
#' @aliases as.data.frame,site-method
#' @exportMethod as.data.frame
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

#' @aliases as.data.frame,sites-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("sites"),
          definition = function(x) {
            x@sites %>% map(as.data.frame) %>% bind_rows() #%>% unique()
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
#' @aliases length,sites-method
#' @exportMethod length
setMethod(f = "length",
          signature = signature(x = "sites"),
          definition = function(x) {
            length(x@sites)
          })

#' @title c - Combine `neotoma2` objects
#' @name c
#' @importFrom methods is
#' @param x `neotoma2` object or NULL
#' @param y `neotoma2` object or NULL
#' @returns concatenated and cleaned `sites` object
#' @md
#' @rdname c
#' @aliases c,sites-method
#' @exportMethod c
setMethod(f = "c",
          signature = "sites",
          definition = function(x, y) {
            if (is(y, "sites")) {
              x@sites <- unlist(c(x@sites, y@sites), recursive = FALSE)
            } else if (is(y, "site")) {
              x@sites <- c(x@sites, y)
            }
            x <- clean(x)
            if (length(x) == 0){
              x <- NULL
            } else {
              x <- new("sites", sites = x@sites)
            }
            return(x)
          }
)

#' @rdname c
setMethod(f = "c",
          signature = "missingOrNULL",
          definition = function(x = "missingORNULL", y) {
            y
          })

#' @title Return the latitude and longitude of sites
#' @param obj A sites object
#' @param ... Additional parameters associated with the call.
#' @returns `data.frame` object with site coordinates.
#' @aliases coordinates,sites-method
#' @exportMethod coordinates
setMethod(f = "coordinates",
          signature = "sites",
          definition = function(obj, ...) {
            coords <- as.data.frame(obj)[, c("siteid", "long", "lat")]
            return(coords)
          })

#' @title Plot site coordinates using a basic plot.
#' @name plot
#' @param x sites object
#' @param y *Ignored.*
#' @param ... Additional parameters associated with the call.
#' @returns `plot` object with site coordinates.
#' @aliases plot,sites-method
#' @exportMethod plot
setMethod(f = "plot",
          signature = "sites",
          definition = function(x, y, ...) {
            coords <- as.data.frame(x)[, c("long", "lat")]
            plot(coords, ...)
          })

#' @aliases plot,site-method
#' @rdname plot
setMethod(f = "plot",
          signature = "site",
          definition = function(x, y, ...) {
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
#' @aliases summary,sites-method
#' @exportMethod summary
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
                  bind_rows() %>%
                  na.omit()
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
              bind_rows() %>%
              rename(collunit_name = .data$collunits.collectionunit,
                     n_chronologies = .data$collunits.chronologies,
                     n_datasets = .data$collunits.datasets,
                     dataset_types = .data$collunits.types)
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
#' tryCatch({
#' ds <- get_datasets(1)
#' doi(ds)
#' }, error = function(e) {
#' message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @aliases doi,sites-method
#' @exportMethod doi
setMethod(f = "doi",
          signature = "sites",
          definition = function(x) {
            map(x@sites, function(y) {
              doi(y)
            }) %>%
              bind_rows()
          })

#' @aliases doi,site-method
#' @rdname doi
setMethod(f = "doi",
          signature = "site",
          definition = function(x) {
            ids <- getids(x)
            ds <- get_datasets(x)
            dois <-
              map(datasets(ds)@datasets,
                  function(x) {
                    doi <- x@doi
                    doi <- unlist(doi, recursive = TRUE, use.names = FALSE)
                    doi <- use_na(testNull(doi), "char")
                    data.frame(datasetid = rep(x$datasetid, length(doi)),
                               doi = doi)
                  }) %>%
              bind_rows()

            dois <- dois %>%
              full_join(ids, by = "datasetid") %>%
              select(.data$siteid, .data$collunitid,
                     .data$datasetid, .data$doi) %>%
              arrange(.data$siteid, .data$collunitid, .data$datasetid)
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
#' tryCatch({
#' ds <- get_datasets(1)
#' cite_data(ds)
#' }, error = function(e) {
#' message("Neotoma server not responding. Try again later.")
#' })
#' }
#' @aliases cite_data,sites-method
#' @exportMethod cite_data
setMethod(f = "cite_data",
          signature = "sites",
          definition = function(x) {
            map(x@sites,
                function(y) {
                  cite_data(y)
                }) %>%
              bind_rows()
          })

#' @aliases cite_data,site-method
#' @rdname cite_data
setMethod(f = "cite_data",
          signature = "site",
          definition = function(x) {
            if (is.null(x)) {
              warning("No sites to filter")
              return(NULL)
            }
            strn <- paste0("%s. %s; %s dataset. ",
                           "In %s. Neotoma Paleoecology Database. doi:%s")
            ids <- getids(x)
            sitenames <- as.data.frame(x)  %>%
              select(.data$siteid, .data$sitename)
            ds <- get_datasets(x)
            ds_df <- as.data.frame(datasets(ds)) %>%
              select(.data$datasetid,
                     .data$datasettype,
                     .data$database)
            dois <-
              map(datasets(ds)@datasets,
                  function(x) {
                    doi <- x@doi
                    doi <- unlist(doi, recursive = TRUE, use.names = FALSE)
                    doi <- use_na(testNull(doi), "char")
                    pi_list <- testNull(unlist(x@pi_list))
                    data.frame(datasetid = rep(x$datasetid, length(doi)),
                               doi = doi,
                               pi_list = rep(paste0(sort(pi_list),
                                                    collapse = "; "),
                                             length(doi)))
                  }) %>%
              bind_rows()

            citations <- dois %>%
              full_join(ids, by = "datasetid") %>%
              full_join(sitenames, by = "siteid") %>%
              full_join(ds_df, by = "datasetid") %>%
              select(.data$siteid, .data$sitename, .data$collunitid,
                     .data$datasetid, .data$datasettype, .data$database,
                     .data$doi, .data$pi_list) %>%
              group_by(.data$siteid, .data$collunitid, .data$datasetid) %>%
              arrange(.data$doi) %>%
              dplyr::filter(row_number() == 1) %>%
              as.data.frame() %>%
              mutate(citation = sprintf(strn, .data$pi_list,
                                        .data$sitename, .data$datasettype,
                                        .data$database, .data$doi)) %>%
              select(.data$datasetid, .data$citation)
            return(citations)
          })

#' @aliases cite_data,NULL-method
#' @rdname cite_data
setMethod(f = "cite_data",
          signature = "NULL",
          definition = function(x) {
            if (is.null(x)) {
              warning("No sites to cite.")
              return(NULL)
            }
          })