#' @title get_manual
#' @author Simon Goring \email{goring@@wisc.edu}
#' @description Open up the Neotoma manual homepage.
#' @importFrom utils browseURL
#' @importFrom rlang is_interactive
#' @examples {
#' # This call does not work from `source()` calls or in testing.
#' # interactive() just lets us know you are interacting with the console:
#' if (interactive()) {
#'  get_manual()
#' }
#' }
#' @returns NULL side effect for opening browser with the manual
#' @md
#' @export
get_manual <- function() {
  if (is_interactive()) {
    browseURL(url = "https://open.neotomadb.org/manual/")
  } else {
    stop("Browser window will not open if interactive() mode is FALSE.")
  }
  return(NULL)
}