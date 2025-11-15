#' @title Build a `site` from the Neotoma API response.
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @param args A list returned from the Neotoma API `data` section.
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @importFrom sf st_read
#' @returns A simple `site` object
#' @noRd
build_site <- function(...) {
  args <- list(...)
  args <- cleanNULL(args)
  geo <- if (!is.null(args$geography) && !is.na(args$geography)) {
    st_read(args$geography, quiet = TRUE)
  } else {
    NULL
  }
  assertthat::assert_that(is.list(args),
                          msg = "Parsed object must be a list.")
  site <- set_site(siteid = use_na(args$siteid, "int"),
                   sitename = use_na(args$sitename, "char"),
                   geography = use_na(geo, "sf"),
                   altitude = use_na(args$altitude, "int"),
                   notes = use_na(args$notes, "char"),
                   description = use_na(args$sitedescription, "char"),
                   collunits = args$collunits)
  return(site)
}
