#' Metrics.
#'
#' Get the metrics of the object.
#'
#' @param object An object to coerce to a list of metrics, or a query object to
#'   replace the metrics of.
#' @param ... Further metrics to add to the resulting list or the replacement
#'   value for the metrics of the query object (if supplied).
#'
#' @export
#' @rdname Metrics
setGeneric(
  "Metrics",
  function(object, ...) {},
  valueClass = ".metrics",
  useAsDefault = FALSE
)

#' Metrics<-.
#'
#' Set the metrics of the object.
#'
#' @param value The replacement dimensions for the supplied object.
#'
#' @rdname Metrics
#' @export
setGeneric(
  "Metrics<-",
  function(object, value) {
    object <- standardGeneric("Metrics<-")
    validObject(object)
    object
  }
)

#' Dimensions.
#'
#' Get the dimensions of the object.
#'
#' @param object An object to be coerced to a list of dimensions.
#' @param ... Other dimensions to add to the returned list, or if \code{object}
#'   is a query object, the replacement dimensions.
#'
#' @export
#' @rdname Dimensions
setGeneric(
  "Dimensions",
  function(object, ...) {},
  valueClass = ".dimensions",
  useAsDefault = FALSE
)

#' Dimensions<-.
#'
#' Set the dimensions for the object.
#'
#' @param value The replacement dimensions for the supplied object.
#'
#' @export
#' @rdname Dimensions
setGeneric(
  "Dimensions<-",
  function(object, value) {
    object <- standardGeneric("Dimensions<-")
    validObject(object)
    object
  }
)

#' SortBy.
#'
#' Get the sortBy order of the query.
#'
#' @param object A character vector or list of dimensions or metrics to sort by.
#'   If character, then prefixing the dimension name with a "+" means ascending
#'   order or "-" for decending order. By default metrics are sorted in
#'   descending order, while dimensions are by default in ascending order.
#'   Alternatively, supply a query object and replacement dimensions and metrics
#'   or sort by.
#' @param ... further dimensions or metrics to sort by, or if \code{object} is a
#'   query then the replacement list of dimensions or metrics to sort by.
#' @param desc A logical vector, same length as the resulting list of dimension
#'   or metric variables, indicating which columns of the resulting query
#'   response should be sorted in decending order.
#'
#' @export
#' @rdname SortBy
setGeneric(
  "SortBy",
  function(object, ..., desc = logical(0)) {},
  valueClass = c(".sortBy", ".query", "NULL"),
  useAsDefault = FALSE
)

#' SortBy<-.
#'
#' Set the order of rows returned by Google Analytics.
#'
#' @param value The replacement dimensions and metrics for the supplied object.
#'
#' @export
#' @rdname SortBy
setGeneric(
  "SortBy<-",
  function(object, value) {
    object <- standardGeneric("SortBy<-")
    validObject(object)
    object
  }
)
