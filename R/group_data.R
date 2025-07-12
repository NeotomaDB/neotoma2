#' @md
#' @title group_data
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @description An internal helper function used to parse the 
#' API response into a hierarchical list of 
#' sites > collectionunits > datasets
#' @param data API response
#' @returns hierarchical `list` with parsed API request
#' @keywords internal
#' @noRd
# TODO This works for downloads. But it should also figure out the 
#different responses in the API, ie datasets or sites calls
group_data <- function(data) {
  # Group by siteid
  site_groups <- split(data, sapply(data, function(x) x$site$siteid))
  
  purrr::map(site_groups, function(site_group) {
    site_info <- site_group[[1]]$site
    
    # collectionunitid
    cu_groups <- split(site_group, sapply(site_group, function(x) {
      x$site$collectionunit$collectionunitid
    }))
    
    # 
    collectionunits <- purrr::imap(cu_groups, function(cu_group, cu_id) {
      cu_info <- cu_group[[1]]$site$collectionunit
      # All datasets for this CU
      cu_info$datasets <- purrr::map(cu_group, ~ .x$site$collectionunit$dataset)
      cu_info
    })

    site_info$collectionunit <- NULL
    site_info$collectionunits <- collectionunits
    
    list(site = site_info)
  })
}