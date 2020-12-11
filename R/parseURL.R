#' @title Make the HTTP request
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @param x The HTTP path for the particular API call.
#' @param dev Should the development API be used (for experimental endpoints), default \code{FALSE}
#' @param ... Any query parameters passed from the function calling \code{parseURL}.
#' @export

parseURL <- function(x, dev = FALSE, ...) {

  if (dev == FALSE) {
    baseurl <- 'https://api.neotomadb.org/v2.0/'
  }  else {
    baseurl <- 'https://api-dev.neotomadb.org/v2.0/'
  }

  response <- httr::GET(paste0(baseurl, x),
    add_headers("User-Agent" = "neotoma2 R package"),
    query = list(...))

  stop_for_status(response,
    task = "Could not connect to the Neotoma API. Check that the path is valid,
            and check the current status of the Neotoma API services at
            http://data.neotomadb.org")

  if (response$status_code == 200) {
    result <- jsonlite::fromJSON(content(response, as = 'text'),
      flatten = FALSE,
      simplifyVector = FALSE)
  }
  return(result)
}
