#' @aliases show,dataset-method
#' @rdname show
setMethod(f = "show",
          signature = "dataset",
          definition = function(object) {
            print(data.frame(datasetid = object@datasetid,
                             database = object@database,
                             datasettype = object@datasettype,
                             age_range_old =  object@age_range_old,
                             age_range_young =  object@age_range_young,
                             age_units = object@age_units,
                             recdatecreated = object@recdatecreated,
                             notes = object@notes), row.names = FALSE)
          })

#' @aliases show,datasets-method
#' @rdname show
setMethod(f = "show",
          signature = "datasets",
          definition = function(object) {
            map(object@datasets, function(y) {
              data.frame(datasetid = y@datasetid,
                         database = y@database,
                         datasettype = y@datasettype,
                         age_range_old =  y@age_range_old,
                         age_range_young =  y@age_range_young,
                         age_units = y@age_units,
                         recdatecreated = y@recdatecreated,
                         notes = y@notes)
            }) %>%
              bind_rows() %>%
              print(row.names = FALSE)
          })

#' @aliases sub-sub,datasets-method
#' @rdname sub-sub
setMethod(f = "[[",
          signature = signature(x = "datasets", i = "numeric"),
          definition = function(x, i) {
            if (length(i) == 1) {
              out <- new("dataset", x@datasets[[i]])
            } else {
              out <- map(i, function(z) {
                new("dataset", x@datasets[[z]])
              })
              out <- new("datasets", datasets = out)
            }
            return(out)
          })

#' @aliases names,dataset-method
#' @rdname names
setMethod(f = "names",
          signature = signature(x = "dataset"),
          definition = function(x) {
            slotNames(x)
          })

#' @aliases sub-subset,datasets-method
#' @rdname sub-subset
setMethod(f = "[[<-",
          signature = signature(x = "datasets"),
          definition = function(x, i, value) {
            datasetset <- x@datasets
            datasetset[[i]] <- value
            out <- new("datasets", datasets = datasetset)
            return(out)
          })

#' @aliases subset,dataset-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "dataset", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @aliases subset,dataset-method
#' @rdname subset
setMethod(f = "[<-",
          signature = signature(x = "dataset", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @aliases cash-set,dataset-method
#' @rdname cash-set
setMethod(f = "$<-",
          signature = signature(x = "dataset"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @aliases sub,datasets-method
#' @rdname sub
setMethod(f = "[",
          signature = signature(x = "datasets", i = "numeric"),
          definition = function(x, i) {
            new("datasets", datasets = x@datasets[i])
          })

#' @aliases cash,dataset-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "dataset"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @aliases cash,datasets-method
#' @rdname cash
setMethod(f = "$",
          signature = signature(x = "datasets"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @aliases as.data.frame,dataset-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("dataset"),
          definition = function(x) {
            data.frame(datasetid = x@datasetid,
                       database = x@database,
                       datasettype = x@datasettype,
                       age_range_old =  x@age_range_old,
                       age_range_young =  x@age_range_young,
                       age_units = x@age_units,
                       recdatecreated = x@recdatecreated,
                       notes = x@notes)
          })

#' @aliases as.data.frame,datasets-method
#' @rdname as.data.frame
setMethod(f = "as.data.frame",
          signature = signature("datasets"),
          definition = function(x) {
            x@datasets %>% map(as.data.frame) %>% bind_rows()
          })

#' @aliases length,datasets-method
#' @rdname length
setMethod(f = "length",
          signature = signature(x = "datasets"),
          definition = function(x) {
            length(x@datasets)
          })

#' @aliases c,datasets-method
#' @rdname c
setMethod(f = "c",
          signature = signature(x = "datasets"),
          definition = function(x, y) {
            new("datasets",
                datasets = unlist(c(x@datasets,
                                    y@datasets), recursive = FALSE))
          })