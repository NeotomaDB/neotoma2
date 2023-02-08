#' @title toWide
#' @param x dataframe object with samples
#' @param variablenames Optional vector to filter by specific variable names.
#' @param ecologicalgroup Vector stating the ecological groups to be filtered by, e.g "DIAT", "TRSH"
#' @param elementtype Label of element type to filter by, e.g. "pollen", "valve"
#' @param unit Label stating which units to filter by, e.g. "NISP"
#' @param groupby Group by 'age' or 'depth'
#' @param operation label or vector of operations to be chosen from: 'prop', 'sum', 
#' 'count', 'presence'.
#' @description Obtain a wide table with information regarding of samples grouped by 
#' variablename and depth/age.
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#' sa <- list(geoJSON = '{"type": "Polygon",
#'        "coordinates": [[
#'             [-79.66, -5.97],
#'             [-70.06, -19.07],
#'             [-74.38, -55.59],
#'             [-34.67, -6.52],
#'             [-76.41, 8.37],
#'             [-79.66, -5.97]
#'             ]]}')
#' 
#' sa$sf <- geojsonsf::geojson_sf(sa$geoJSON)
#' 
#' sa_sites <- neotoma2::get_sites(loc = sa$sf, all_data=TRUE)
#' sa_datasets <- neotoma2::get_datasets(sa_sites)
#' sa_diatom <- sa_datasets %>% 
#'  neotoma2::filter(datasettype == "diatom" & !is.na(age_range_young))
#' sa_dl <- sa_diatom %>% get_downloads()
#' plottingSite <- sa_dl[[1]]
#' 
#' # Obtain variablenames via taxa()
#' sa_taxa <- taxa(plottingSite) %>%
#'  filter(ecologicalgroup %in% c("DIAT")) %>%
#'  filter(elementtype == "valve") 
#' var_names <- sa_taxa$variablename
#' 
#'}
toWide <- function(x, variablenames=c(), ecologicalgroups=NA, elementtypes=NA, 
                   unit=NA, groupby='age', operation='prop') {
  
  if(is.null(variablenames)){
    df <- x
    warning("All available names in the provided samples data frame will be included.")
  } else {
    df <- x %>%
      dplyr::filter(variablename %in% variablenames)
  }
  
  if(is.na(ecologicalgroups)){
    stop("Please provide which ecological groups you want to filter by.")
  }
  
  if(is.na(elementtypes)){
    stop("Please provide which element types you want to filter by.")
  }
  
  if(is.na(unit)){
    stop("Please provide which units you want to filter by.")
  }
  
  df <- df %>% 
    dplyr::filter(ecologicalgroup %in% ecologicalgroups) %>%
    dplyr::filter(elementtype == elementtypes) %>%
    dplyr::filter(units == unit)
  
  # Get proportion values
  onesite <- df %>%
    dplyr::group_by_at(groupby) %>%
    dplyr::mutate(counter = sum(value, na.rm = TRUE)) %>%
    dplyr::group_by(variablename) %>% 
    dplyr::mutate(prop = value / counter) %>% 
    dplyr::arrange(desc(groupby))
  
  widetable <- onesite %>%
    dplyr::mutate(prop = as.numeric(prop),
                  sum = as.numeric(sum(value)),
                  counter = as.numeric(counter),
                  presence = case_when(
                    counter > 0 ~ 1,
                    counter == 0 ~ 0)) %>%
    dplyr::select(all_of(groupby), variablename, all_of(operation))
  
  counts <- tidyr::pivot_wider(widetable,
                               id_cols = groupby,
                               names_from = variablename,
                               values_from = operation,
                               values_fill = 0,
                               values_fn=mean)
  
  return(counts)
}