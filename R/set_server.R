#' @title Set Neotoma API Source or Server
#' @author Simon Goring \email{goring@wisc.edu}
#' @importFrom assertthat assert_that
#' @param server One of \code{local} (when the API is running locally on
#' port 3005), \code{neotoma} or \code{dev}.
#' @examples \donttest{
#' # The user is running the API locally using the node/express API
#' # cloned from github: https://github.com/NeotomaDB/api_nodetest
#' set_server(server = "local")
#' # The user switches back to the remote api server.
#' set_server(server = "neotoma")
#' }
#' @returns NULL modifies how to talk to the API (local, dev, server)
#' @description
#' Choose to pull Neotoma data from the main Neotoma server, the development
#' server or from a local instance of the API.
#' @export
set_server <- function(server = "neotoma") {
  assert_that(server %in% c("neotoma", "dev", "local"),
              msg = "The parameter `server` must be `local`, 
                    `neotoma` or `dev`.")
  server <- switch(server,
                   neotoma = "neotoma",
                   dev = "dev",
                   local = "local")

  Sys.setenv("APIPOINT" = server)
}
