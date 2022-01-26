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
#' @export
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
    
    sample_id <- x$samples[[j]]$sampleid
    df <- x$samples[[j]]$datum %>%
      map(function(y) {
        as.data.frame(y)
      }) %>%
      bind_rows()
    
    df_age <- x$samples[[j]]$ages %>%
      map(function(y) {
        as.data.frame(y)
      }) %>%
      bind_rows()
    
    # Other data
    df$isgn <- x$samples[[j]]$igsn
    df$depth <- x$samples[[j]]$depth
    df$sampleid <- x$samples[[j]]$sampleid
    df$thickness <- x$samples[[j]]$thickness
    df$samplename <- x$samples[[j]]$samplename
    df$analysisunitid <- x$samples[[j]]$analysisunitid
    df$analysisunitname <- x$samples[[j]]$analysisunitname
    
    new_df <- cbind(df, df_age)
    
    df_sample <- new_df
    
    taxon_table <- rbind(taxon_table, df_sample) 
    
    # Analyst Info
    
    analyst_list_helper <- x$samples[[j]]$sampleanalyst %>%
      map(function(y) {
        y$contactname
      })
    
    analyst_list <- c(analyst_list_helper)
  }
  
  # PI Information
  pi_list <- testNull(x$datasetpi, list())
  if(length(pi_list) != 0) {
    pi_list <- pi_list %>%
      map(function(y) {
        if(is.na(y[1])){
          NA_character_
        }else{
        y$contactname
          }
      })
  }

  new("dataset",
      datasetid = use_na(testNull(x$datasetid, NA), "int"),
      database = use_na(testNull(x$database, NA), "char"),
      doi = list(x$doi),
      datasettype = use_na(testNull(x$datasettype, NA), "char"),
      age_range_old = use_na(testNull(x$agerange[[1]]$ageold, NA), "int"),
      age_range_young = use_na(testNull(x$agerange[[1]]$ageyoung, NA), "int"),
      notes = use_na(testNull(x$datasetnotes, NA), "char"),
      pi_list = pi_list,
      taxa_table = taxon_table,
      analyst = analyst_list)
  
}
