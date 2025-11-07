#' @rdname show
#' @export
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

#' @rdname show
#' @export
setMethod(f = "show",
          signature = "datasets",
          definition = function(object) {
            map(object@datasets, function(y) {
              df <- data.frame(datasetid = y@datasetid,
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

#' @rdname sub-sub
#' @export
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

#' @rdname names
#' @export
setMethod(f = "names",
          signature = signature(x = "dataset"),
          definition = function(x) {
            slotNames(x)
          })

#' @rdname sub-subset
#' @export
setMethod(f = "[[<-",
          signature = signature(x = "datasets"),
          definition = function(x, i, value) {
            datasetset <- x@datasets
            datasetset[[i]] <- value
            out <- new("datasets", datasets = datasetset)
            return(out)
          })

#' @rdname subset
#' @export
setMethod(f = "[<-",
          signature = signature(x = "dataset", i = "character"),
          definition = function(x, i, value) {
            for (idx in 1:length(i)) {
              slot(x, i[idx]) <- value[idx]
            }
            return(x)
          })

#' @rdname subset
#' @export
setMethod(f = "[<-",
          signature = signature(x = "dataset", i = "numeric"),
          definition = function(x, i, value) {
            slots <- slotNames(x)
            for (idx in 1:length(i)) {
              slot(x, slots[i[idx]]) <- value[idx]
            }
            return(x)
          })

#' @rdname cash-set
#' @export
setMethod(f = "$<-",
          signature = signature(x = "dataset"),
          definition = function(x, name, value) {
            slot(x, name) <- value
            return(x)
          })

#' @rdname sub
#' @export
setMethod(f = "[",
          signature = signature(x = "datasets", i = "numeric"),
          definition = function(x, i) {
            new("datasets", datasets = x@datasets[i])
          })

#' @rdname cash
#' @export
setMethod(f = "$",
          signature = signature(x = "dataset"),
          definition = function(x, name) {
            slot(x, name)
          })

#' @rdname cash
#' @export
setMethod(f = "$",
          signature = signature(x = "datasets"),
          definition = function(x, name) {
            x %>%
              map(function(y) {
                slot(y, name)
              }) %>%
              unlist()
          })

#' @rdname as.data.frame
#' @export
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

#' @rdname as.data.frame
#' @export
setMethod(f = "as.data.frame",
          signature = signature("datasets"),
          definition = function(x) {
            x@datasets %>% map(as.data.frame) %>% bind_rows()
          })

#' @rdname length
#' @export
setMethod(f = "length",
          signature = signature(x = "datasets"),
          definition = function(x) {
            length(x@datasets)
          })

#' @rdname c
#' @export
setMethod(f = "c",
          signature = signature(x = "datasets"),
          definition = function(x, y) {
            new("datasets",
                datasets = unlist(c(x@datasets,
                                    y@datasets), recursive = FALSE))
          })