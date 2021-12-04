#' @title get_downloads
#' @author Socorro Dominguez \email{sedv8808@@gmail.com}
#' @import gtools
#' @import lubridate
#' @import dplyr
#' @importFrom methods new
#' @description
#' Helper function to build a dataset
#' @param x dataset list
#' @return list parsed into datasets
#' @examples \dontrun{
#' # To build dataset from API call:
#' build_dataset(x)
#' }
#' 
build_dataset <- function(x) {
  
  # Taxon Table
  taxon_table <- c()
  length_datum <- length(x$samples)
  analyst_list <- list()
  for (j in seq_len(length_datum)) {
    depth <- x$samples[[j]]$depth
    sample_id <- x$samples[[j]]$sampleid
    df <- x$samples[[j]]$datum %>%
      map(function(y) {
        as.data.frame(y)
      }) %>%
      bind_rows()
    
    df_sample <- df %>%
      select(variablename, units, element, taxongroup, ecologicalgroup, taxonid)
    taxon_table <- rbind(taxon_table, df_sample) %>%
      distinct()
    
    # Analyst Info
    
    analyst_list_helper <- x$samples[[j]]$sampleanalyst %>%
      map(function(y) {
        y$contactname
      })
    
    analyst_list <- c(analyst_list_helper)
  }
  
  # PI Information
  pi_list <- x$datasetpi %>%
    map(function(y) {
      y$contactname
    })
  
  new("dataset",
      datasetid = use_na(x$datasetid, "int"),
      database = use_na(x$database, "char"),
      doi = list(x$doi),
      datasettype = use_na(x$datasettype, "char"),
      age_range_old = use_na(x$agerange[[1]]$ageold, "int"),
      age_range_young = use_na(x$agerange[[1]]$ageyoung, "int"),
      notes = use_na(x$datasetnotes, "char"),
      pi_list = pi_list,
      taxa_table = taxon_table,
      analyst = analyst_list)
  
}
