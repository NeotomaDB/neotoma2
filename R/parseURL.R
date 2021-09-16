#' @title Make the HTTP request
#' @description An internal helper function used to connect to the Neotoma API
#' in a standard manner, and to provide basic validation of any response.
#' @param x The HTTP path for the particular API call.
#' @param use By default use the neotoma server (\code{neotoma}), but supports either the development API server or a local server.
#' @param ... Any query parameters passed from the function calling \code{parseURL}.
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' 
#' @export

parseURL <- function(x, use = 'neotoma', ...) {

  clean <- function(x) {
    ifelse(is.null(x), NA, x)
  }
  
  cleanNull <- function(x, fn = function(x) if(is.null(x)) NA else x)
  {
    if(is.list(x)) {
      lapply(x, cleanNull, fn)
    } else {
      fn(x)
    }
  }
  
  baseurl <- switch(use,
                    "dev" = "https://api-dev.neotomadb.org/v2.0/",
                    "neotoma" = "https://api.neotomadb.org/v2.0/",
                    "local" = "http://localhost:3005/v2.0/",
                    use)

  response <- httr::GET(paste0(baseurl, x),
    add_headers("User-Agent" = "neotoma2 R package"),
    query = list(...))
  
  response_url <- response$url

  # When need to check API endpoint, uncomment below
  #print("Response's URL \n")
  #print(response$url)
  
  cl <- as.list(match.call())

  stop_for_status(response,
    task = "Could not connect to the Neotoma API. Check that the path is valid,
            and check the current status of the Neotoma API services at
            http://data.neotomadb.org")

  if (response$status_code == 200) {
    result <- jsonlite::fromJSON(httr::content(response, as = 'text'),
      flatten = FALSE,
      simplifyVector = FALSE)
  }
  
  
  pager(result, response_url)
  # Remove if debug done
  #print(result)
  return(cleanNull(result))
}
