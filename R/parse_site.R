#' @title parse_site
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom purrr map map_lgl
#' @importFrom methods new
#' @description An internal helper function used to parse site information into
#' `neotoma2R` objects.
#'
#' `group_response()` has already folded whichever endpoint answered into a
#' single shape, so this function only walks that structure
#' (*site* > *collectionunit* > *dataset* > *sample*) and hands each piece of
#' the response to the matching `build_*()` function. The API field names are
#' mapped there, once, and nowhere else.
#' @param result The API response.
#' @param verbose If TRUE print progress to console in bar form.
#' @returns `list` with cleaned and parsed data from HTTP request
#' @noRd
parse_site <- function(result, verbose = FALSE) {
  data <- group_response(result$data)
  new_sites <- map(data, function(x) {
    if (verbose) {
      cat(".")
    }
    cus <- map(x$site$collectionunits, function(y) {
      ds <- map(y$datasets, function(z) {
        samples <- map(z$samples, build_sample)
        samples <- samples[!map_lgl(samples, is.null)]
        z$samples <- new("samples", samples = samples)
        build_dataset(z)
      })
      # Drop malformed/empty datasets (build_dataset() returns NULL for them)
      # before building the container, so we never keep a phantom dataset.
      ds <- ds[!map_lgl(ds, is.null)]
      y$datasets <- new("datasets", datasets = ds)
      chronologies <- map(y$chronologies, build_chron)
      chronologies <- chronologies[!map_lgl(chronologies, is.null)]
      y$chronologies <- new("chronologies", chronologies = chronologies)
      y$speleothems <- tryCatch(
        parse_speleothem(y$speleothems),
        error = function(e) {
          NULL # Speleothems does not exist in the regular API calls
        }
      )
      build_collunits(y)
    })
    # Drop malformed/empty collection units (build_collunits() returns NULL for
    # them) before building the container.
    cus <- cus[!map_lgl(cus, is.null)]
    x$site$collunits <- new("collunits", collunits = cus)
    build_site(x$site)
  })
  # Drop malformed/empty sites (build_site() returns NULL for them) before
  # building the container.
  new_sites <- new_sites[!map_lgl(new_sites, is.null)]
  new_sites <- new("sites", sites = new_sites)
  return(new_sites)
}
