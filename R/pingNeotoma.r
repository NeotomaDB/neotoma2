#' @title pingNeotoma
#' @importFrom httr HEAD
#' @importFrom stringr str_detect
#' @importFrom assertthat assert_that
#' @description A quick function to test whether or not the Neotoma Database
#' API is currently running.
#' @param server One of \code{localhost:PORT} (where \code{PORT} is a valid
#' numeric port), \code{neotoma} or \code{dev}.
#' @returns A valid HTTP status code or returns an error if a connection
#' is refused.
#' @examples {
#' test_connection <- pingNeotoma("neotoma")
#' }
#' @export
pingNeotoma <- function(server = "neotoma") {
  valid_local <- str_detect(server,
                            "(http?:////){0,1}localhost:\\d{1,5}$")
  assert_that(valid_local | (server %in% c("neotoma", "dev")),
              msg = "The parameter `server` must be a valid 
                    localhost (e.g., localhost:3005), or `neotoma` or `dev`.")
  server <- switch(server,
                   neotoma = "https://api.neotomadb.org",
                   dev = "https://api-dev.neotomadb.org",
                   server)
  status <- HEAD(server)
  return(status)
}