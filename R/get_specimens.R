#' @title get_specimens
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @import gtools
#' @import lubridate
#' @import geojsonsf
#' @importFrom methods new
#' @description
#' Information for Specimens
#' @param x Use a single specimenid
#' @param ... Additional terms passed to get_specimens, most common datasetid
#' @returns The function returns a specimens list
#' @keywords internal
#' @noRd
get_specimens <- function(x = NA, ...) {
  if (!missing(x)) {
    UseMethod("get_specimens", x)
  }else {
    UseMethod("get_specimens", NA)
  }
}

# Parse specimen must take the result of API call + sites object
parse_specimen <- function(result, ds) {

  sps <- result$data %>%
    cleanNULL()

  sp_index <- purrr::map(sps, function(x) {
    data.frame(datasetid = x$datasetid)}) %>%
    dplyr::bind_rows()

  # Move from get_downloads to a different helper function script
  check_match <- function(sp_row, ids) {
    apply(ids, 1, function(x) sum(sp_row == x))
  }

  for (i in 1:length(sps)) {

    ids <- getids(ds, order = FALSE)
    matches <- check_match(sp_index[i,], ids)

    if (max(matches) == 1) {
      # Retrieve IDs for site and collectionunit based on datasetID
      st <- match(ids$siteid[which.max(matches)], unique(ids$siteid))

      cuids <- ids %>%
        dplyr::filter(siteid == unique(ids$siteid)[st], .preserve = TRUE)

      cuid <- which(unique(cuids$collunitid) == ids$collunitid[which.max(matches)])

      # Filter based on datasetID
      dsids <- cuids %>%
        dplyr::filter(collunitid == unique(cuids$collunitid)[cuid], .preserve = TRUE)

      dsid <- which(unique(dsids$datasetid) == sp_index$datasetid[i])

      newsp <- build_specimen(sps[[i]])

      # Attach built specimen slot to datasets

      datasets <- ds[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]]

      datasets@specimens@specimens <- c(datasets@specimens@specimens,
                                        newsp)

      datasets@samples@samples <- ds[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]]@samples@samples

      ds[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]] <- datasets

    }
  }
  return(ds)
}

#' @title Get Specimen Numeric
#' @param x Use a single number to extract site information
#' @param ... Additional terms passed to get_specimens.
#' @returns The function returns a specimens list
#' @keywords internal
#' @noRd
# remove meanwhile
get_specimens.numeric <- function(x, ...) {

  if (length(x) > 0) {
    specimenid <- paste0(x, collapse = ",")
  }

  base_url <- paste0("data/specimens/", specimenid)
  result <- parseURL(base_url)


  if(length(result$data) ==0){
    stop("Specimen ID not found. If you meant dataset ID, use parameter datasetid")
  }
  sps <- result$data %>%
    cleanNULL()

  #if sps is empty list, exit and return error No Specimen ID available.
  
  sp_index <- purrr::map(sps, function(x) {
    data.frame(datasetid = x$datasetid)}) %>%
    dplyr::bind_rows()

  dw <- get_downloads(sp_index$datasetid)

  ds <- parse_specimen(result, dw)

  return(ds)

}

#' @title Get Specimen datasetid
#' @param ... Pass argument datasetid and the corresponding datasetid
#' @returns The function returns a specimens list
#' @keywords internal
#' @noRd
# remove meanwhile
get_specimens.default <- function(...) {
 
  cl <- as.list(match.call())
  cl[[1]] <- NULL
  
  cl <- lapply(cl, eval, envir = parent.frame())
  dsid <- as.numeric(cl$datasetid)

  if (length(dsid) > 0) {
    dsid <- paste0(dsid, collapse = ",")
  }
  
  
  base_url <- paste0("data/datasets/", as.character(dsid), "/specimens")
  result <- parseURL(base_url)
  
  dw <- get_downloads(cl$datasetid)
  ds <- parse_specimen(result, dw)
  
  return(ds)
}


#' @title Get Specimen Sites
#' @param x Use a single number to extract site information
#' @param ... Other possible parameters such as datasetid
#' @returns The function returns a specimens list
#' @keywords internal
#' @noRd
# remove meanwhile
get_specimens.sites <- function(x,...) {

  output <- getids(x) %>%
    dplyr::select(datasetid) %>%
    stats::na.omit() %>%
    unique() %>%
    unlist()

  if (length(output) > 0) {
    output <- paste0(output, collapse = ",")
  }

  base_url <- paste0("data/datasets/", output,"/specimens/")
  result <- parseURL(base_url)

  df <- suppressWarnings(samples(x))

  if(dim(df)[1] == 0){
    x <- get_downloads(x)
  }

  ds <- parse_specimen(result, x)

  return(ds)
}