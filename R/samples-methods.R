#' @title  Slicer
#' @param x samples object
#' @param i iteration in samples list
#' @description Obtain one of the elements within a samples list
#' @export
setMethod(f = "[[",
          signature = signature(x = "samples", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("sample", x@samples[[i]])
            } else {
              out <- purrr::map(i, function(z) {
                new("sample", x@samples[[z]])
              })
              out <- new("samples", samples = out)
            }
            return(out)
          })

#' @title  $
#' @param x sample object
#' @param name name of the slot
#' @description Obtain slots of a sample without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "sample"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @title  $ for samples
#' @param x samples object
#' @param name name of the slot
#' @description Obtain slots of a site without using at-mark
#' @export
setMethod(f = "$",
          signature = signature(x = "samples"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })


#' @title Length Method samples
#' @export
#' @param x samples object
setMethod(f = "length",
          signature = signature(x = "samples"),
          definition = function(x) {
            length(x@samples)
          })

#' @title  as.data.frame sample
#' @param x sample object
#' @description show as dataframe
#' @export
setMethod(f = "as.data.frame",
          signature = signature("sample"),
          definition = function(x) {
            data.frame(igsn = x@igsn,
                       depth = x@depth,
                       sampleid = as.character(x@sampleid),
                       thickness = x@thickness,
                       samplename = x@samplename,
                       analysisunitid = as.character(x@analysisunitid),
                       analysisunitname = x@analysisunitname)
          })

#' @title  Show the sample information
#' @param object sample object
#' @export
setMethod(f = "show",
          signature = signature(object = "sample"),
          definition = function(object) {
            result <- as.data.frame(object)
            print(result)
          })

#' @title Hash a sample object
#' @description Hash a Neotoma sample object
#' @param object sample object
#' @importFrom cli hash_obj_sha256
#' @examples
#' some_site <- get_sites(sitename = "Site%")
#' hash(some_site[[1]]$collunit[[1]]$dataset[[1]]$samples[[1]])
#' @export
setMethod(f = "hash",
          signature = "sample",
          definition = function(x) {
            df <- data.frame(igsn = x@igsn,
                             depth = x@depth,
                             sampleid = as.character(x@sampleid),
                             thickness = x@thickness,
                             samplename = x@samplename,
                             analysisunitid = as.character(x@analysisunitid),
                             analysisunitname = x@analysisunitname)
            
            output <- hash_obj_sha256(df)
            return(output)
          }
)