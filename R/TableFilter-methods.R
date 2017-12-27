#' @include TableFilter-generics.R
#' @include table-filter-classes.R
#' @include table-filter-coerce.R
#' @include utils.R
#' @importFrom methods setMethod
NULL

#' @describeIn TableFilter Return the TableFilter that has been applied to the
#'   given query, or coerce the given object into a table filter.
setMethod("TableFilter", "ANY", function(object) {as(object, ".tableFilter")})

setMethod("TableFilter", "gaQuery", function(object) {
  object@filters
})

setMethod("TableFilter", c("gaQuery", "ANY"), function(object, value) {
  use_class <- class(object@filters)
  object@filters <- as(value, use_class)
  validObject(object)
  object
})

#' @describeIn TableFilter Method to replace the table filter of a query
setMethod(
  f = "TableFilter<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, ".tableFilter") <- value
    object
  }
)

