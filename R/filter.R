#' @title filter
#' @import sf
#' @import dplyr
#' @import purrr
#' @import stringr
#' @param x A site, dataset or download.
#' @param ... arguments to filter by: latmin, latmax
#' longmin, longmax, elevmin, elevmax, datasettype.
#' @export

filter <- function(x, ...) {
  UseMethod("filter")
}

#' @export
filter.sites <- function(x, ...) {  # nolint
  # Naming the Dots
  cl <- as.list(match.call())
  cl[1] <- NULL
  cl$x <- NULL

  calls_list <- c()
  for (i in seq_len(length(cl))) {
    txt <- paste0(cl[[i]], collapse = "")
    if (str_detect(txt, "lat")) {
      # Appending latitude call
      my_list <- list(lat = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
    if (str_detect(txt, "long")) {
      # Appending longitude call
      my_list <- list(long = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
    if (str_detect(txt, "elev")) {
      # Appending elevation call
      my_list <- list(elev = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
    if (str_detect(txt, "datasettype")) {
      # Appending datasettype call
      my_list <- list(datasettype = cl[[i]])
      calls_list <- append(calls_list, my_list)
    }
  }

  for (i in names(calls_list)) {
    if (!(i %in% c("lat", "long", "elev", "datasettype", "loc")))
      message(paste0(i, " is not a valid parameter.
       It will not be used for filtering"))
  }
  sites <- c()

  for (i in seq_len(length(x@sites))) {
    coll_units <- c()
    for (j in seq_len(length(x@sites[[i]]@collunits@collunits))) {
      datasets_list <- c()
      datasets <- c()
      datasets_call <- x@sites[[i]]@collunits@collunits[[j]]@datasets
      for (k in seq_len(length(datasets_call@datasets))) {
        datasets_lat <- c()
        datasets_long <- c()
        datasets_elev <- c()
        datasets_type <- c()
        # Check for type
        if ("datasettype" %in% names(calls_list)) {
          datasettype <- datasets_call@datasets[[k]]@datasettype # nolint
          dataset <- datasets_call@datasets[[k]]
          if (eval(calls_list$datasettype) == TRUE) {
            datasets_type <- append(datasets_type, dataset)
          }else{
            datasets_type <- append(datasets_type, 0)
          }
        }

        # Check for lat
        if ("lat" %in% names(calls_list)) {
          lat <- sf::st_coordinates(x@sites[[i]]@geography)[, 2][1] # nolint
          dataset <- datasets_call@datasets[[k]]
          if (eval(calls_list$lat) == TRUE) {
            datasets_lat <- append(datasets_lat, dataset)
          }else{
          datasets_lat <- append(datasets_lat, 0)
          }
        }

        # Check for long
        if ("long" %in% names(calls_list)) {
          long <- sf::st_coordinates(x@sites[[i]]@geography)[,1][1] # nolint
          dataset <- datasets_call@datasets[[k]]
          if (eval(calls_list$long) == TRUE) {
            datasets_long <- append(datasets_long, dataset)
          }else{
            datasets_long <- append(datasets_long, 0)
          }
        }

        # Check for elev
        if ("elev" %in% names(calls_list)) {
          elev <- x@sites[[i]]@altitude # nolint
          dataset <- datasets_call@datasets[[k]]
          if (eval(calls_list$elev) == TRUE) {
            datasets_elev <- append(datasets_elev, dataset)
          }else{
            datasets_elev <- append(datasets_elev, 0)
          }
        }

        # TODO: Check for loc element
        datasets_names <- list(lat = datasets_lat,
                               long = datasets_long,
                               elev = datasets_elev,
                               datasettype = datasets_type)

        if (!("datasettype" %in% names(calls_list))) {
          datasets_names$datasettype <- NULL
          }

        if (!("lat" %in% names(calls_list))) {
          datasets_names$lat <- NULL
          }

        if (!("long" %in% names(calls_list))) {
          datasets_names <- purrr::list_modify(datasets_names, "long" = NULL)
        }
        if (!("elev" %in% names(calls_list))) {
          datasets_names <- purrr::list_modify(datasets_names, "elev" = NULL)
        }

        ch_dataset <- Reduce(intersect, datasets_names)
        datasets <- append(datasets, ch_dataset)

        if (length(datasets) != 0) {
          if (!(0 %in% datasets)) {
            datasets_list <- new("datasets", datasets = datasets)
            }
        }else{
          datasets_list <- NULL
          }
          }
      if (!is.null(datasets_list)) {
        collunit <- x@sites[[i]]@collunits@collunits
        coll_units <- append(coll_units, collunit)
        coll_units <- new("collunits", collunits = coll_units)
        }
    }
    if (!is.null(datasets_list)) {
      new_site <- new("site",
                      siteid = x@sites[[i]]@siteid,
                      sitename = x@sites[[i]]@sitename,
                      geography = x@sites[[i]]@geography,
                      altitude = x@sites[[i]]@altitude,
                      description = "description",
                      notes = NA_character_,
                      collunits = coll_units)
      sites <- append(sites, new_site)
    }
  }

  if (!is.null(sites)) {
    sites <- new("sites", sites = sites)
    }

  if (exists("sites")) {
    return(sites)
    }else{
      return(message("No datasets match your conditions."))
      }
}