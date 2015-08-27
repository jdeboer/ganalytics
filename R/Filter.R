#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include utils.R
NULL

#' @describeIn TableFilter
setMethod("TableFilter", ".query", function(object) {as(object, ".tableFilter")})

#' @describeIn TableFilter
setMethod("TableFilter", "NULL", function(object) {as(object, ".tableFilter")})

#' @describeIn TableFilter
setMethod("TableFilter", ".tableFilter", function(object) {object})

#' @describeIn TableFilter
setMethod("TableFilter", ".compoundExpr", function(object) {as(object, ".tableFilter")})

#' @describeIn TableFilter
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

#' @describeIn TableFilter
setMethod(
  f = "TableFilter<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, ".tableFilter") <- value
    object
  }
)

# Backwards compatibility
#' @export GaFilter
#' @rdname TableFilter
GaFilter <- TableFilter

#' @export GaFilter<-
#' @rdname TableFilter
`GaFilter<-` <- `TableFilter<-`
