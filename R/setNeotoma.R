#' @title Set Neotoma API Source
#' @importFrom stringr str_detect
#' @importFrom assertthat assertthat
#' param server One of \code{local} (when the API is running locally on port 3005),
#' \code{neotoma} or \code{dev}.
#' @description Choose to pull Neotoma data from the main Neotoma server,
#' the development server or from a local instance of the API.
#' @examples 
#' # The user is running the API locally using the node/express API
#' # cloned from github: https://github.com/NeotomaDB/api_nodetest
#' setNeotoma("local")
#' @export

setNeotoma <- function(server = "neotoma") {

    assertthat::assert_that(server %in% c("neotoma", "dev", "local"),
        msg = "The parameter `server` must be `local`, `neotoma` or `dev`.")

    server <- switch(server,
        neotoma = "neotoma",
        dev = "dev",
        "local")

    Sys.setenv(server)
}