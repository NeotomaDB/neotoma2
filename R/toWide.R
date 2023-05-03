#' @title toWide
#' @param x dataframe object with samples
#' @param variablenames Optional vector to filter by specific variable names.
#' @param ecologicalgroups Vector stating the ecological groups to be filtered by, e.g "DIAT", "TRSH"
#' @param elementtypes Label of element type to filter by, e.g. "pollen", "valve"
#' @param unit Label stating which units to filter by, e.g. "NISP"
#' @param groupby Group by 'age' or 'depth'
#' @param operation label or vector of operations to be chosen from: 'prop', 'sum', 
#' 'presence'.
#' @description Obtain a wide table with information regarding of samples grouped by 
#' variablename and depth/age.
#' @export
#' @import dplyr
#' @returns wide `data.frame` obtained from long `samples` `data.frame`
#' @examples
#' \donttest{
#' fourcorners <- '{"type": "Polygon",
#' "coordinates": [[
#' [-109.36060497194846, 37.69552879956651],
#' [-107.813845732192, 37.69552879956651],
#' [-107.813845732192, 36.80303716260222],
#' [-109.36060497194846, 36.80303716260222], 
#' [-109.36060497194846, 37.69552879956651]
#' ]]}'
#'
#' # Download all vertebrate localities within a bounding box.
#' fc_sites <- neotoma2::get_sites(loc = fourcorners[1])
#' fc_ds <- neotoma2::get_datasets(fc_sites) %>% 
#' neotoma2::filter(datasettype=="vertebrate fauna")
#' 
#' fc_dl <- neotoma2::get_downloads(fc_ds)
#' fc_dl1 <- fc_dl[[1]]
#' 
#' fc_smp <- samples(fc_dl1)
#' toWide(fc_smp, ecologicalgroups=c('AVES', 'RODE'), 
#' elementtypes='bone/tooth', unit='present/absent')
#' 
#'}
toWide <- function(x, variablenames=c(), ecologicalgroups=c(), elementtypes=c(), 
                   unit=c(), groupby='age', operation='prop') {
  
  if(is.null(variablenames)){
    df <- x
    warning("All available variable names in the provided samples data frame will be included.")
  } else {
    df <- x %>%
      dplyr::filter(variablename %in% variablenames)
  }
  
  if(is.null(ecologicalgroups)){
    stop("Please provide which ecological groups you want to filter by.")
  }
  
  if(is.null(elementtypes)){
    stop("Please provide which element types you want to filter by.")
  }
  
  if(is.null(unit)){
    stop("Please provide which units you want to filter by.")
  }
  
  df <- df %>% 
    dplyr::filter(ecologicalgroup %in% ecologicalgroups) %>%
    dplyr::filter(elementtype %in% elementtypes) %>%
    dplyr::filter(units %in% unit)
  
  # Get proportion values
  onesite <- df %>%
    dplyr::group_by_at(groupby) %>%
    dplyr::mutate(counter = sum(value, na.rm = TRUE)) %>%
    dplyr::group_by(variablename) %>% 
    dplyr::mutate(prop = value / counter,
                  n = value) %>% 
    dplyr::arrange(desc(groupby))
  
  if(unit=="present/absent"){
    if(operation != "presence"){
      warning("Unit is `present/absent`, operation 'presence' will be applied.")
    }
    operation="presence"
  }
  widetable <- onesite %>%
    dplyr::mutate(prop = as.numeric(prop),
                  sum = as.numeric(value),
                  presence = case_when(
                    counter > 0 ~ 1,
                    counter == 0 ~ 0)) %>%
    dplyr::select(all_of(groupby), variablename, all_of(operation))

  counts <- tidyr::pivot_wider(widetable,
                               id_cols = groupby,
                               names_from = variablename,
                               values_from = operation,
                               values_fill = 0,
                               values_fn=sum)
  
  return(counts)
}