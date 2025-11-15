#' @title get_documentation
#' @author Socorro Dominguez \email{dominguezvid@@wisc.edu}
#' @description Open up the Neotoma R homepage.
#' @importFrom utils browseURL
#' @importFrom rlang is_interactive
#' @returns NULL
#' @examples \donttest{
#' if (interactive()) {
#'  get_documentation()
#' }
#' }
#' @md
#' @export
get_documentation <- function() {
  if (is_interactive()) {
    browseURL(url = "https://open.neotomadb.org/neotoma2/")
  } else {
    stop("Browser window will not open if interactive() mode is FALSE.")
  }
  return(NULL)
}