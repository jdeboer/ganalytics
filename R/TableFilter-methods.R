#' @include TableFilter-generics.R
#' @include table-filter-classes.R
#' @include table-filter-coerce.R
#' @include utils.R
#' @importFrom methods setMethod
NULL

#' @describeIn TableFilter Return the TableFilter that has been applied to the
#'   given query
setMethod("TableFilter", ".query", function(object) {as(object, ".tableFilter")})

#' @describeIn TableFilter Returns NULL
setMethod("TableFilter", "NULL", function(object) {as(object, ".tableFilter")})

#' @describeIn TableFilter Returns itself
setMethod("TableFilter", ".tableFilter", function(object) {object})

#' @describeIn TableFilter Coerce an expression into a table filter.
setMethod("TableFilter", ".compoundExpr", function(object) {as(object, ".tableFilter")})

#' @describeIn TableFilter Coerce a segment into a table filter. Only possible
#'   for segments without sequential conditions.
setMethod("TableFilter", "gaDynSegment", function(object) {as(object, ".tableFilter")})

#' @describeIn TableFilter
setMethod(
  f = "TableFilter<-",
  signature = c(".tableFilter", "andExpr"),
  definition = function(object, value) {
    as(object, "andExpr") <- value
    object
  }
)

#' @describeIn TableFilter Method to replace the table filter of a query
setMethod(
  f = "TableFilter<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, ".tableFilter") <- value
    object
  }
)

