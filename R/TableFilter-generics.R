#' TableFilter
#'
#' Get the filter.
#'
#' @param object The object to be coerced to a TableFilter or the query object
#'   to apply a table filter to.
#' @param value The replacement table filter where \code{object} is a query.
#'
#' @family query object functions
#'
#' @export
#' @rdname TableFilter
setGeneric(
  "TableFilter",
  function(object, value) {standardGeneric("TableFilter")},
  valueClass = c(".tableFilter", ".query")
)

#' TableFilter<-
#'
#' Set the filter.
#'
#' @family query object functions
#'
#' @export
#' @rdname TableFilter
setGeneric(
  "TableFilter<-",
  function(object, value) {
    object <- standardGeneric("TableFilter<-")
    validObject(object)
    object
  }
)
