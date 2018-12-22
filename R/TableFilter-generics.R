#' TableFilter.
#'
#' Get the filter.
#'
#' @param object The object to be coerced to a TableFilter or the query object
#'   to apply a table filter to.
#' @param value The replacement table filter where \code{object} is a query.
#'
#' @export
#' @rdname TableFilter
setGeneric(
  "TableFilter",
  function(object, value) {standardGeneric("TableFilter")},
  valueClass = c(".tableFilter", ".query")
)

#' TableFilter<-.
#'
#' Set the filter.
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
