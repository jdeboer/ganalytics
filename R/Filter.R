#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
NULL

setMethod("TableFilter", ".query", function(object) {as(object, ".tableFilter")})

setMethod("TableFilter", "NULL", function(object) {as(object, ".tableFilter")})

setMethod("TableFilter", ".tableFilter", function(object) {object})

setMethod("TableFilter", ".compoundExpr", function(object) {as(object, ".tableFilter")})

setMethod("TableFilter", "gaDynSegment", function(object) {as(object, ".tableFilter")})

setMethod(
  f = "TableFilter<-",
  signature = c(".tableFilter", "andExpr"),
  definition = function(object, value) {
    as(object, "andExpr") <- value
    object
  }
)

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
GaFilter <- TableFilter
#' @export GaFilter<-
`GaFilter<-` <- `TableFilter<-`
