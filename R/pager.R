#' @title pager
#' @description An internal helper function to count through all offsets/limits
#' @param x The result of the API call
#' @param counter If TRUE, modify the response to contain all datapoints
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @import roperators
#' @export

pager <- function(response, response_url, complete_response = FALSE, ...) {
  
  responses <- c()
  cl <- as.list(match.call())
  
  if("offset" %in% names(cl)){
    param_offset <- cl$offset
  }else{
    param_offset <- 1
  }
  
  param_limit <- 500
  
  result <- response$data
  
  param_offset = length(response$data)
  param_offset_old = 0
  
  responses <- append(responses, result)
  while((length(result) > 0) & param_offset_old != param_offset){
    if(grepl("\\?", response_url)){
      response <- httr::GET(paste0(response_url, '&offset=', param_offset, '&limit=500'))
    }else{
      response <- httr::GET(paste0(response_url, '?offset=', param_offset, '&limit=500'))
    }

    if (response$status_code == 200) {
      result <- jsonlite::fromJSON(httr::content(response, as = 'text'),
                                   flatten = FALSE,
                                   simplifyVector = FALSE)
    }
    new_response_url <- response$url
    param_offset_old = param_offset
    param_offset %+=% length(result$data)
    
    responses <- append(responses, result$data)
    
  }

  
  if(complete_data==TRUE){
    message(paste0("Your search returned ", param_offset-1, " objects."))
    return(responses)
  }else{
    message(paste0("Your search returned ", param_offset-1, " objects."))
  }
  
}