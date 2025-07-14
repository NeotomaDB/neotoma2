#' @md
#' @title Build a `site` from the Neotoma API response.
#' @param args A list returned from the Neotoma API `data` section.
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @import sf
#' @returns A simple `site` object
#' @keywords internal
#' @noRd
build_site <- function(...) {
  args <- list(...)
  geo <- if (!is.null(args$geography) && !is.na(args$geography)) {
    sf::st_read(args$geography, quiet = TRUE)
  } else {
    NULL
  }
  assertthat::assert_that(is.list(args),
                          msg = "Parsed object must be a list.")
 site <- set_site(siteid = use_na(testNull(args$siteid, NA), "int"),
             sitename = use_na(testNull(args$sitename, NA), "char"),
             geography = use_na(testNull(geo, st_as_sf(st_sfc())), "sf"),
             altitude = use_na(testNull(args$altitude, NA), "int"),
             notes = use_na(testNull(args$notes, NA), "char"),
             description = use_na(testNull(args$sitedescription, NA), "char"),
             collunits = args$collunits)
  return(site)
}
