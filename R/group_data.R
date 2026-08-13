#' @title group_data
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map map_lgl
#' @description An internal helper function used to parse the
#' API response into a hierarchical list of
#' *sites* > *collectionunits* > *datasets*
#'
#' The three endpoints hand back the same information in three different
#' shapes, and each of them can repeat a collection unit:
#'
#' * `sites` returns one element per site, whose `collectionunits` repeats the
#'   same unit once per dataset.
#' * `datasets` returns one element per collection unit, with the unit's fields
#'   flattened onto the site.
#' * `downloads` returns one element per dataset, with the unit nested under
#'   `site$collectionunit`.
#'
#' `group_response()` flattens all three into the single shape
#' `list(site = <site>)`, where `site$collectionunits` is a list of unique
#' collection units and each unit carries every dataset that belongs to it.
#' It only rearranges the response; the API field names are left untouched so
#' that the `build_*()` functions stay the one place that maps them.
#' @param data API response
#' @returns hierarchical `list` with parsed API request
#' @noRd
group_response <- function(data) {
  if (length(data) == 0) {
    return(list())
  }
  # Detect endpoint
  is_dataset  <- all(map_lgl(data, ~ !is.null(.x$site$datasets)))
  is_download <- all(map_lgl(data, function(x) {
    tryCatch(!is.null(x$site$collectionunit$dataset), error = function(e) FALSE)
  }))
  is_site <- all(map_lgl(data, ~ !is.null(.x$collectionunits)))
  if (is_download) {
    elements <- map(data, split_download)
  } else if (is_dataset) {
    elements <- map(data, split_dataset)
  } else if (is_site) {
    elements <- map(data, split_site)
  } else {
    stop("Unrecognized data structure.")
  }
  group_sites(elements)
}

#' @title split_site
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal function used to split one element of a `sites`
#' response into its site and its collection units. The units already hold
#' their datasets, so only the site needs to be separated out.
#' @param x One element of the API `data` section.
#' @returns `list` with a `site` and its `collectionunits`.
#' @noRd
split_site <- function(x) {
  site <- x
  site$collectionunits <- NULL
  list(site = site, collectionunits = x$collectionunits)
}

#' @title split_dataset
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal function used to split one element of a `datasets`
#' response. This endpoint flattens the collection unit onto the site, so the
#' unit's fields are lifted back out into a unit of their own.
#' @param x One element of the API `data` section.
#' @returns `list` with a `site` and its `collectionunits`.
#' @noRd
split_dataset <- function(x) {
  site <- x$site
  cu_fields <- c("collectionunitid", "collectionunit", "handle", "unittype")
  cu <- site[intersect(cu_fields, names(site))]
  cu$datasets <- site$datasets
  site[c(cu_fields, "datasets")] <- NULL
  list(site = site, collectionunits = list(cu))
}

#' @title split_download
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal function used to split one element of a `downloads`
#' response. Each element carries a single dataset under a single collection
#' unit, so the dataset is lifted into the `datasets` list the other endpoints
#' use.
#' @param x One element of the API `data` section.
#' @returns `list` with a `site` and its `collectionunits`.
#' @noRd
split_download <- function(x) {
  site <- x$site
  cu <- site$collectionunit
  cu$datasets <- list(cu$dataset)
  cu$dataset <- NULL
  site$collectionunit <- NULL
  site$dataset <- NULL
  list(site = site, collectionunits = list(cu))
}

#' @title group_sites
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @description An internal function that gathers split response elements into
#' unique sites. Elements sharing a `siteid` become one site, collection units
#' sharing a `collectionunitid` become one unit, and that unit keeps every
#' unique dataset the API reported against it.
#' @param elements A list of `list(site =, collectionunits =)` elements.
#' @returns `list` of `list(site = )`, one per unique site.
#' @noRd
group_sites <- function(elements) {
  site_groups <- split_by_id(elements, map_chr_id(elements, function(x) {
    x$site$siteid
  }))
  unname(map(site_groups, function(site_group) {
    site <- site_group[[1]]$site
    cus <- unlist(map(site_group, "collectionunits"), recursive = FALSE)
    site$collectionunits <- group_collunits(cus)
    list(site = site)
  }))
}

#' @title group_collunits
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map
#' @description An internal function that folds repeated collection units into
#' one. The API repeats a unit once per dataset, so units sharing a
#' `collectionunitid` are merged and their datasets pooled, keeping each
#' `datasetid` once.
#' @param cus A list of collection units from the API.
#' @returns `list` of unique collection units, each holding all its datasets.
#' @noRd
group_collunits <- function(cus) {
  if (length(cus) == 0) {
    return(list())
  }
  cu_groups <- split_by_id(cus, map_chr_id(cus, function(x) {
    testNull(x$collectionunitid, x$handle)
  }))
  unname(map(cu_groups, function(cu_group) {
    cu <- cu_group[[1]]
    datasets <- unlist(map(cu_group, "datasets"), recursive = FALSE)
    cu$datasets <- unique_datasets(datasets)
    cu
  }))
}

#' @title unique_datasets
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal function that keeps the first copy of each
#' `datasetid`, so that a unit merged from several response elements does not
#' report the same dataset twice.
#' @param datasets A list of datasets from the API.
#' @returns `list` of datasets with unique `datasetid`s.
#' @noRd
unique_datasets <- function(datasets) {
  datasets <- datasets[!map_lgl(datasets, is.null)]
  if (length(datasets) == 0) {
    return(list())
  }
  ids <- map_chr_id(datasets, function(x) x$datasetid)
  datasets[!duplicated(ids)]
}

#' @title split_by_id
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal helper that groups elements by an identifier while
#' keeping the order the API sent them in. `split()` on its own would sort the
#' groups by identifier, which would quietly reorder the sites a query returns.
#' `exclude = NULL` keeps a missing identifier as a group of its own rather than
#' dropping those records on the floor.
#' @param x A list of API elements.
#' @param ids A `character` vector of identifiers, one per element.
#' @returns `list` of groups, in order of first appearance.
#' @noRd
split_by_id <- function(x, ids) {
  split(x, factor(ids, levels = unique(ids), exclude = NULL))
}

#' @title map_chr_id
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom purrr map_chr
#' @description An internal helper that pulls an identifier out of each element
#' as a character, so that it can be used to `split()` or de-duplicate. A
#' missing identifier becomes `NA_character_` rather than an error.
#' @param x A list of API elements.
#' @param f A function returning the identifier for one element.
#' @returns `character` vector of identifiers.
#' @noRd
map_chr_id <- function(x, f) {
  map_chr(x, function(y) {
    id <- tryCatch(f(y), error = function(e) NULL)
    if (is.null(id) || length(id) == 0) NA_character_ else as.character(id[1])
  })
}
