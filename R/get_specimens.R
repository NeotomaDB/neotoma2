# #' @title get_specimens
# ##' @name get_specimens
# #' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
# #' @importFrom purrr map
# #' @importFrom dplyr bind_rows select filter
# #' @description
# #' Information for Specimens
# #' @param x Use a single specimenid
# #' @param ... Additional terms passed to get_specimens, most common datasetid
# #' @returns The function returns a specimens list
# #' @keywords internal
# #' @noRd
# get_specimens <- function(x, ...) {
#   UseMethod("get_specimens")
# }

# parse_specimen <- function(result, ds) {
#   sps <- result$data %>%
#     cleanNULL()
#   sp_index <- map(sps,
#                   function(x) {
#                     data.frame(datasetid = x$datasetid)
#                   }) %>%
#                 bind_rows()

#   # Move from get_downloads to a different helper function script
#   check_match <- function(sp_row, ids) {
#     apply(ids, 1, function(x) sum(sp_row == x))
#   }
#   for (i in seq_along(sps)) {
#     ids <- getids(ds, order = FALSE)
#     matches <- check_match(sp_index[i, ], ids)
#     if (max(matches) == 1) {
#       # Retrieve IDs for site and collectionunit based on datasetID
#       st <- match(ids$siteid[which.max(matches)], unique(ids$siteid))
#       cuids <- ids %>%
#         filter(siteid == unique(ids$siteid)[st], .preserve = TRUE)
#       cuid <- which(unique(
#                     cuids$collunitid) == ids$collunitid[which.max(matches)])
#       # Filter based on datasetID
#       dsids <- cuids %>%
#         filter(collunitid == unique(cuids$collunitid)[cuid], .preserve = TRUE)
#       dsid <- which(unique(dsids$datasetid) == sp_index$datasetid[i])
#       newsp <- build_specimen(sps[[i]])

#       # Attach built specimen slot to datasets
#       datasets <- ds[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]]
#       datasets@specimens@specimens <- c(datasets@specimens@specimens,
#                                         newsp)
#       datasets@samples@samples <-
#         ds[[st]]@collunits@collunits[[
#           cuid]]@datasets@datasets[[dsid]]@samples@samples
#       ds[[st]]@collunits@collunits[[cuid]]@datasets@datasets[[dsid]] <- datasets
#     }
#   }
#   return(ds)
# }

# ##' @rdname get_specimens
# ##' @export
# # remove meanwhile
# get_specimens.numeric <- function(x, ...) {
#   if (length(x) > 0) {
#     specimenid <- paste0(x, collapse = ",")
#   }

#   baseURL <- paste0("data/specimens/", specimenid)
#   result <- tryCatch(
#     parseURL(baseURL),
#     error = function(e) {
#       message("API call failed: ", e$message)
#       NULL
#     }
#   )
#   if ((is.null(result)) | (length(result$data) == 0)) {
#     stop("Specimen ID not found. 
#          If you meant dataset ID, use parameter datasetid")
#   }
#   sps <- result$data %>%
#     cleanNULL()
#   #if sps is empty list, exit and return error No Specimen ID available.
#   sp_index <- map(sps, function(x) {
#     data.frame(datasetid = x$datasetid)}) %>%
#     bind_rows()
#   dw <- get_downloads(sp_index$datasetid)
#   ds <- parse_specimen(result, dw)
#   return(ds)

# }

# ##' @rdname get_specimens
# ##' @export
# get_specimens.default <- function(...) {
#   cl <- as.list(match.call())
#   cl[[1]] <- NULL
#   cl <- lapply(cl, eval, envir = parent.frame())
#   dsid <- as.numeric(cl$datasetid)
#   if (length(dsid) > 0) {
#     dsid <- paste0(dsid, collapse = ",")
#   }
#   baseURL <- paste0("data/datasets/", as.character(dsid), "/specimens")
#   result <- tryCatch(
#     parseURL(base_url, ...),
#     error = function(e) {
#       message("API call failed: ", e$message)
#       NULL
#     }
#   )
#   if(length(result$data) == 0) {
#     warning("No specimens found for the provided dataset ID.")
#     return(NULL)
#   } else {
#     dw <- get_downloads(cl$datasetid)
#     ds <- parse_specimen(result, dw)
#     return(ds)
#   }
# }

# ##' @rdname get_specimens
# ##' @export
# get_specimens.sites <- function(x,...) {
#   output <- getids(x) %>%
#     select(datasetid) %>%
#     stats::na.omit() %>%
#     unique() %>%
#     unlist()
#   if (length(output) > 0) {
#     output <- paste0(output, collapse = ",")
#   }
#   baseURL <- paste0("data/datasets/", output, "/specimens/")
#   result <- tryCatch(
#     parseURL(baseURL, ...),
#     error = function(e) {
#       message("API call failed: ", e$message)
#       NULL
#     }
#   )
#   if (length(result$data) == 0) {
#     warning("No specimens found for the provided dataset ID.")
#     return(NULL)
#   } else {
#     df <- suppressWarnings(samples(x))
#     if(dim(df)[1] == 0){
#       x <- get_downloads(x)
#     }
#     ds <- parse_specimen(result, x)
#     return(ds)
#   }
# }