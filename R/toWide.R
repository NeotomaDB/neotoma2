#' @title toWide
#' @author Socorro Dominguez \email{dominguezvid@wisc.edu}
#' @importFrom dplyr filter mutate arrange select group_by_at group_by
#' @importFrom tidyr pivot_wider
#' @param x dataframe object with samples
#' @param variablenames Optional vector to filter by specific variable names.
#' @param ecologicalgroups Vector stating the ecological groups to be
#' filtered by, e.g "DIAT", "TRSH"
#' @param elementtypes Label of element type to filter by,
#' e.g. "pollen", "valve"
#' @param unit Label stating which units to filter by, e.g. "NISP"
#' @param groupby Group by 'age' or 'depth'
#' @param operation label or vector of operations to be chosen from:
#' 'prop', 'sum', 'presence'.
#' @returns wide `data.frame` obtained from long `samples` `data.frame`
#' @examples
#' \donttest{
#' fc_sites <- neotoma2::get_datasets(limit=5, datasettype = "vertebrate fauna")
#' fc_ds <- fc_sites %>%
#'    neotoma2::get_downloads()
#' fc_dl1 <- fc_dl[[1]]
#' fc_smp <- samples(fc_dl1)
#' toWide(fc_smp, ecologicalgroups=c('AVES', 'RODE'),
#'        elementtypes='bone/tooth', unit='present/absent')
#'}
#' @description Obtain a wide table with information regarding of
#' samples grouped by variablename and depth/age.
#' @export
toWide <- function(x,
                   variablenames = NA,
                   ecologicalgroups = NA,
                   elementtypes = NA,
                   unit = NA,
                   groupby = "age",
                   operation = "prop") {
  if (!is.na(variablenames)) {
    x <- x %>% filter(variablename %in% variablenames)
  }
  if (!is.na(ecologicalgroups)) {
    x <- x %>% filter(ecologicalgroup %in% ecologicalgroups)
  }
  if (!is.na(elementtypes)) {
    x <- x %>% filter(elementtype %in% elementtypes)
  }
  if (is.na(unit)) {
    x <- x %>% filter(units %in% unit)
  }
  # Get proportion values
  onesite <- x %>%
    group_by_at(groupby) %>%
    mutate(counter = sum(value, na.rm = TRUE)) %>%
    group_by(variablename) %>% 
    mutate(prop = value / counter,
           n = value) %>%
    arrange(desc(groupby))
  if (unit == "present/absent") {
    if (operation != "presence") {
      warning("Unit is `present/absent`, operation 'presence' will be applied.")
    }
    operation <- "presence"
  }
  widetable <- onesite %>%
    mutate(prop = as.numeric(prop),
           sum = as.numeric(value),
           presence = case_when(counter > 0 ~ 1,
                                counter == 0 ~ 0)) %>%
    select(all_of(groupby), variablename, all_of(operation))
  counts <- pivot_wider(widetable,
                        id_cols = groupby,
                        names_from = variablename,
                        values_from = operation,
                        values_fill = 0,
                        values_fn = sum)
  return(counts)
}