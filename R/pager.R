#' @title pager
#' @description An internal helper function to count through all offsets/limits
#' @param x The result of the API call
#' @param counter If TRUE, modify the response to contain all datapoints
#' @importFrom httr add_headers content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' 
#' @export

pager <- function(response, complete_response = FALSE, ...) {
  
  responses <- c()
  cl <- as.list(match.call())

  response_url <- response$url
  
  if("offset" %in% names(cl)){
    param_offset <- cl$offset
  }else{
    param_offset <- 1
  }
  
  if("limit" %in% names(cl)){
    param_limit <- cl$limit
  }else{
    param_limit <- 500
  }
  
  
  total_responses <- param_limit
  
  if (response$status_code == 200) {
    result <- jsonlite::fromJSON(httr::content(response, as = 'text'),
                                 flatten = FALSE,
                                 simplifyVector = FALSE)
  }
  
  responses <- append(responses, result)
  
  print("length of data")
  print(length(result$data))

  while(length(result$data)>0){
    
    if(grepl("offset", response_url)){
      response_url <- response_url %>% stringr::str_remove(paste0("\\?offset=", param_offset))
    }
    
    if(grepl("limit", response_url)){
      response_url <- response_url %>% stringr::str_remove(paste0("limit=", param_limit, "&"))
    }
    
    param_offset <- param_offset + param_limit
    
    response2 <- httr::GET(paste0(response_url, '?offset=', param_offset, '&limit=', param_limit))
    print(response2$url)
    
    
    result <- jsonlite::fromJSON(httr::content(response2, as = 'text'),
                                  flatten = FALSE,
                                  simplifyVector = FALSE)
    
    
    responses <- append(responses, result)
    
    total_responses <- total_responses + length(result$data)
    print("total responses in the loop")
    print(total_responses)
  }

  print("total responses")
  print(total_responses)
  
  if(complete_response == TRUE){
    message(paste0("Complete response of ", total_responses, " elements."))
    return(responses)
  }else{
    return(message(paste0("This object contains ", total_responses, " elements.")))
  }
}