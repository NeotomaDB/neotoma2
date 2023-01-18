#' @title samples
#' @param x sites object
#' @description Obtain wide samples table
#' @export
#' @import dplyr
setMethod(f = "toWide",
          signature = "sites",
          definition = function(x) {
            output <- purrr::map(x@sites, function(y) toWide(y)) %>%
              dplyr::bind_rows()
            return(output)
          }
)

#' @title samples
#' @param x sites object
#' @description Obtain elements on the samples level
#' @export
#' @import dplyr
setMethod(f = "toWide",
          signature = "site",
          definition = function(x) {
            # Extract samples
            shortSamples <- samples(x)
            
            # Get proportion values
            onesite <- shortSamples %>%
              dplyr::group_by(age) %>%
              dplyr::mutate(counter = sum(value, na.rm = TRUE)) %>%
              dplyr::group_by(variablename) %>% 
              dplyr::mutate(prop = value / counter) %>% 
              dplyr::arrange(desc(age))
            
            widetable <- onesite %>%
              dplyr::select(age, variablename, prop) %>% 
              dplyr::mutate(prop = as.numeric(prop))
      
            return(widetable)
          }
)