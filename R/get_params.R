#' @title swagger_cache
#' @description A session-level cache for the API's Swagger definition. Every
#' `get_*()` call validates its arguments through `get_params()`, which without
#' a cache re-downloads and re-scans the whole `swagger-ui-init.js` bundle on
#' each call -- a full extra HTTP round trip before any data is requested.
#' The document changes only when the API is redeployed, so fetching it once per
#' session is enough.
#' @noRd
swagger_cache <- new.env(parent = emptyenv())

#' @title get_swagger
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom httr GET content
#' @description An internal helper that returns the API's Swagger definition,
#' downloading it at most once per session. A failed download is not cached, so
#' a transient outage does not disable argument checking for the rest of the
#' session.
#' @returns `character` body of the Swagger document.
#' @noRd
get_swagger <- function() {
  if (!is.null(swagger_cache$body)) {
    return(swagger_cache$body)
  }
  url <- "https://api.neotomadb.org/api-docs/swagger-ui-init.js"
  body <- content(GET(url), "text", encoding = "UTF-8")
  swagger_cache$body <- body
  body
}

#' @title get_params
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom stringr str_match str_match_all
#' @keywords internal
get_params <- function(obj) {
  cached <- swagger_cache[[paste0("params_", obj)]]
  if (!is.null(cached)) {
    return(cached)
  }
  r <- tryCatch({
    result <- get_swagger()
    sw <- str_match(result,
                    paste0("v2.0/data/", obj,
                           "\"\\s*:\\s*(\\{[\\s\\S]+?\\})\\s*\\n"))[, 2]
    matches <- str_match_all(
      sw, "#/components/parameters/([a-zA-Z0-9_]+)Query\\\""
    )[[1]][, 2]
    matches <- unique(matches)
    matches
  }, error = function(e) {
    warning("Swagger with available parameters not available in API: ",
            conditionMessage(e))
    NULL
  })
  params <- append(r, list("all_data", "limit", "offset"))
  params <- lapply(params, tolower)
  # Only a successful lookup is cached; a failed one must be retried.
  if (!is.null(r)) {
    swagger_cache[[paste0("params_", obj)]] <- params
  }
  return(params)
}