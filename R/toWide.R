#' @title toWide
#' @param x dataframe object with samples
#' @param groupby User can groupby 'age' or 'depth'
#' @param operation label or vector of operations to be chosen from: 'prop', 'sum', 
#' 'count', 'presence'.
#' @description Obtain a wide table with information regarding of samples grouped by 
#' variablename and depth/age.
#' @export
#' @import dplyr
toWide <- function(x, groupby='age', operation='prop') {
            # Get proportion values
            onesite <- x %>%
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
              dplyr::select(all_of(groupby), variablename, all_of(operation)) %>%
              tidyr::drop_na(sum)

            return(widetable)
          }