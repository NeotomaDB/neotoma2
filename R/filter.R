#' @title Apply a `filter` for Neotoma sites objects.
#' @name filter
#' @author Simon Goring \email{goring@wisc.edu}
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description The \code{filter} function takes a \code{sites} object
#' and allows a user to filter on a number of properties. Since a sites object
#' is a nested object (it contains collection units, datasets, samples, etc.)
#' the degree to which filtering occurs depends on the amount of data contained
#' within the sites object. Filtering parameters include:
#'  * `siteid` A numeric site identifier from the Neotoma Database.
#'  * `sitename` The character string sitename.
#'  * `lat` A numeric latitude value.
#'  * `long` A numeric longitude value.
#'  * `altitude` The elevation of the site. Note that some sites do not
#'   include elevation information. For these an NA value appears, and they
#'   would be removed when using an elevation filter.
#'  * `datasetid` A numeric datasetid from Neotoma.
#'  * `database` A character string naming the constituent database
#'   from which the dataset is drawn.
#'  * `datasettype` A character string representing one of the many
#'   dataset types within Neotoma.
#'  * `age_range_old` A dataset-level parameter indicating the oldest
#'   date covered by the dataset chronology.
#'  * `age_range_young` A dataset-level parameter indicating the youngest
#'   date covered by the dataset chronology.
#'  * `notes` Free-form dataset notes provided by the dataset PI(s),
#'   analysts or data stewards.
#'  * `collectionunitid` A numeric collection unit identifier from
#'   Neotoma.
#'  * `handle` A character string identifying the collection unit. These
#'   are often shorter form names (originally a default 8 character length).
#'  * `collectionunitname` A character string identifying the collection
#'   unit name.
#'  * `colldate` The date on which the collection unit was sampled. Many
#'   of these are empty.
#'  * `location` A free-form character string indicating the location of
#'   the collection unit within the site.
#'  * `waterdepth` A numeric depth at which the core was obtained.
#'  * `collunittype` A character string for the collection unit type.
#'  * `collectiondevice` A fixed vocabulary term for the collection
#'   device.
#'  * `depositionalenvironment` A fixed vocabulary name for the
#'   depositional environment.
#'  * `loc` A spatial filter. Keeps only sites whose geography falls within
#'   the supplied region. The region may be an `sf`/`sfc`/`sfg` object, a
#'   GeoJSON string, a WKT string (e.g. `"POLYGON ((...))"`), or a numeric
#'   bounding box `c(xmin, ymin, xmax, ymax)`, e.g.
#'   `filter(altitude < 100, loc == my_polygon)`. `geography` is accepted
#'   as an alias for `loc`.
#' @importFrom dplyr filter inner_join
#' @importFrom rlang enquos quo_get_expr quo_get_env eval_tidy new_quosure
#' @importFrom sf st_geometry st_intersects st_union st_crs st_transform
#' @importFrom sf st_sfc st_as_sfc st_bbox st_is_empty st_geometrycollection
#' @importFrom geojsonsf geojson_sf
#' @param .data A site, dataset, download, or data frame
#' @param ... Additional arguments passed to `filter()`
#' @param .by (only used for filtering `data.frame` objects)
#' @param .preserve (only used for filtering `data.frame` objects)
#' @returns filtered `sites` object
#' @examples \dontrun{
#' # Download 10 sites, but only keep the sites that are close to sea level.
#' tryCatch({
#'  some_sites <- get_sites(sitename = "Lake%", limit = 3)
#'   site_subset <- some_sites %>% filter(altitude < 100)
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.")
#' })
#' # Download 10 sites, get all associated datasets, but keep only
#' # sites/datasets that are of datasettype "pollen":
#' tryCatch({
#'   sites <- get_sites(limit = 10) %>%
#'     get_datasets()
#'   pollen_subset <- sites %>% filter(datasettype == "pollen")
#' }, error = function(e) {
#'   message("Neotoma server not responding. Try again later.") 
#' })
#' }
#' @md
#' @export
filter <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  UseMethod("filter")
}

#' @rdname filter
#' @exportS3Method filter NULL
filter.NULL <- function(.data, ...) {
  warning("No sites to filter")
  return(NULL)
}

#' @rdname filter
#' @exportS3Method filter sites
filter.sites <- function(.data, ...) {
  x <- .data
  # Capture every condition in `...` as a quosure so we can (a) inspect all of
  # them when deciding which levels to join and (b) splice only the ordinary
  # column conditions back into `dplyr::filter()` after peeling off any spatial
  # `loc` condition, which is handled separately below.
  quosures <- rlang::enquos(...)
  exprs <- lapply(quosures, rlang::quo_get_expr)
  # `loc` is not a real column; it is a spatial filter on site geography.
  # `geography` is accepted as an alias for `loc` (it is the name of the site
  # slot the filter actually acts on), so `filter(geography == poly)` behaves
  # exactly like `filter(loc == poly)`.
  spatial_keywords <- c("loc", "geography")
  is_loc <- vapply(exprs,
                   function(e) any(spatial_keywords %in% all.vars(e)),
                   logical(1))
  loc_quos <- quosures[is_loc]
  other_quos <- quosures[!is_loc]
  # Collect the referenced column names from ALL ordinary conditions, not just
  # the first. `all.vars()` returns the actual variable names used in each
  # expression, so detection keys on real columns rather than substrings of the
  # deparsed text (which would false-match on things like "lat" in "latitude"
  # or "long" inside a string literal such as "LONGCORE").
  referenced_vars <- unique(unlist(lapply(exprs[!is_loc], all.vars)))
  sitecols <- any(c("sitename", "lat", "long", "altitude") %in%
                    referenced_vars)
  datasetcols <- any(c("datasetid", "database", "datasettype", "age_range_old",
                       "age_range_young", "notes") %in% referenced_vars)
  collunitcols <- any(c("collectionunitid", "handle", "colldate",
                        "location", "waterdepth", "collunittype",
                        "collectiondevice", "defaultchronology",
                        "collectionunitname", "depositionalenvironment") %in%
                        referenced_vars)
  ids <- getids(x)
  if (length(loc_quos) > 0) {
    inside_ids <- loc_site_ids(x, loc_quos)
    ids <- ids[ids$siteid %in% inside_ids, , drop = FALSE]
  }
  if (sitecols == TRUE) {
    ids <- ids %>%
      inner_join(as.data.frame(x), by = "siteid") %>%
      rename(altitude = .data$elev,
             sitenotes = .data$notes)
  }
  if (collunitcols == TRUE) {
    ids <- ids %>%
      inner_join(as.data.frame(collunits(x)),
                 by = c("collunitid" = "collectionunitid"))
  }
  if (datasetcols == TRUE) {
    ids <- ids %>%
      inner_join(mutate(as.data.frame(datasets(x)),
                        datasetid = as.numeric(.data$datasetid)),
                 by = "datasetid")
  }
  cleanids <- ids %>%
    dplyr::filter(!!!other_quos)
  if (nrow(cleanids) == 0) {
    return(new("sites"))
  }
  siteids <- unique(as.data.frame(x)$siteid)
  pared_sites <- x[which(siteids %in% cleanids$siteid)]
  # Clear datasets:
  good_dsid <- unique(cleanids$datasetid)
  good_cuid <- unique(cleanids$collunitid)
  pared_ds <- purrr::map(pared_sites@sites, function(x) {
    ycu <- collunits(x)
    ycu <- ycu[which(as.data.frame(ycu)$collectionunitid %in% good_cuid)]
    xcu <- purrr::map(ycu@collunits, function(y) {
      yds <- datasets(y)
      yds <- yds[which(as.data.frame(yds)$datasetid %in% good_dsid)]
      y@datasets <- yds
      return(y)
    })
    x@collunits@collunits <- xcu
    return(x)
  })
  return(new("sites", sites = pared_ds))
}

#' @title loc_region
#' @description Turn the right-hand side of a `loc == <region>` filter condition
#' into a single WGS84 (EPSG:4326) `sfc` polygon. Accepts an `sf`/`sfc`/`sfg`
#' object, a GeoJSON character string, a WKT character string, or a numeric
#' bounding box `c(xmin, ymin, xmax, ymax)`.
#' @param region The evaluated region value from the `loc` condition.
#' @returns An `sfc` geometry in EPSG:4326.
#' @keywords internal
#' @noRd
loc_region <- function(region) {
  if (inherits(region, "sf")) {
    geom <- sf::st_geometry(region)
  } else if (inherits(region, "sfc")) {
    geom <- region
  } else if (inherits(region, "sfg")) {
    geom <- sf::st_sfc(region)
  } else if (is.character(region)) {
    if (is_wkt(region)) {
      geom <- sf::st_as_sfc(region, crs = 4326)
    } else {
      geom <- sf::st_geometry(geojsonsf::geojson_sf(region))
    }
  } else if (is.numeric(region)) {
    if (length(stats::na.omit(region)) != 4) {
      stop("A numeric `loc` needs 4 values: c(xmin, ymin, xmax, ymax).")
    }
    bbox <- sf::st_bbox(c(xmin = region[1], ymin = region[2],
                          xmax = region[3], ymax = region[4]),
                        crs = sf::st_crs(4326))
    geom <- sf::st_as_sfc(bbox)
  } else {
    stop("`loc` must be an sf/sfc/sfg object, a GeoJSON string, or a ",
         "numeric bounding box c(xmin, ymin, xmax, ymax).")
  }
  if (is.na(sf::st_crs(geom))) {
    sf::st_crs(geom) <- 4326
  }
  sf::st_transform(geom, 4326)
}

#' @title loc_site_ids
#' @description Return the `siteid`s whose geography intersects the region(s)
#' described by one or more `loc` filter conditions. Each quosure is expected to
#' be of the form `loc == <region>`; the non-`loc` operand is evaluated in the
#' quosure's environment and coerced with `loc_region()`. Multiple `loc`
#' conditions are unioned.
#' @param x A `sites` object.
#' @param loc_quos A list of quosures referencing `loc`.
#' @returns A numeric vector of matching `siteid`s.
#' @keywords internal
#' @noRd
loc_site_ids <- function(x, loc_quos) {
  regions <- lapply(loc_quos, function(q) {
    e <- rlang::quo_get_expr(q)
    env <- rlang::quo_get_env(q)
    operands <- as.list(e)[-1L]
    keep <- !vapply(operands,
                    function(o) {
                      is.name(o) &&
                        as.character(o) %in% c("loc", "geography")
                    },
                    logical(1))
    region_expr <- operands[keep][[1]]
    loc_region(rlang::eval_tidy(rlang::new_quosure(region_expr, env)))
  })
  region <- sf::st_union(do.call(c, regions))
  sids <- vapply(x@sites, function(s) s@siteid, numeric(1))
  site_geoms <- lapply(x@sites, function(s) {
    g <- sf::st_geometry(s@geography)
    if (length(g) == 0 || all(sf::st_is_empty(g))) {
      return(sf::st_sfc(sf::st_geometrycollection(), crs = 4326))
    }
    g <- sf::st_union(g)
    if (is.na(sf::st_crs(g))) {
      sf::st_crs(g) <- 4326
    }
    sf::st_transform(g, 4326)
  })
  site_sfc <- do.call(c, site_geoms)
  hits <- sf::st_intersects(site_sfc, region, sparse = FALSE)[, 1]
  sids[hits]
}

#' This is a re-export of \code{dplyr::filter} for data frames.
#' @rdname filter
#' @exportS3Method filter data.frame
filter.data.frame <- getS3method("filter",
                                 "data.frame",
                                 envir = asNamespace("dplyr"))