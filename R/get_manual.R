#' @title get_manual
#' @author Simon Goring \email{goring@@wisc.edu}
#' @description Open up the Neotoma manual homepage.
#' @examples
#' get_manual()
#' @importFrom utils browseURL
#' @export
get_manual <- function() {
  if (interactive()) {
    browseURL(url = "https://open.neotomadb.org/manual")
  } else {
    stop("Browser window will not open if interactive() mode is FALSE.")
  }
  return(NULL)
}
