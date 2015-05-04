#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
NULL

setMethod("TableFilter", ".query", function(.Object) {as(.Object, ".tableFilter")})

setMethod("TableFilter", "NULL", function(.Object) {as(.Object, ".tableFilter")})

setMethod("TableFilter", ".tableFilter", function(.Object) {.Object})

setMethod("TableFilter", ".compoundExpr", function(.Object) {as(.Object, ".tableFilter")})

setMethod("TableFilter", "gaDynSegment", function(.Object) {as(.Object, ".tableFilter")})

setMethod(
  f = "TableFilter<-",
  signature = c(".tableFilter", "andExpr"),
  definition = function(.Object, value) {
    as(.Object, "andExpr") <- value
    .Object
  }
)

setMethod(
  f = "TableFilter<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    as(.Object, ".tableFilter") <- value
    .Object
  }
)

# Backwards compatibility
#' @export GaFilter
GaFilter <- TableFilter
#' @export GaFilter<-
`GaFilter<-` <- `TableFilter<-`

# Forwards compatibility
#' @export Filter
Filter <- TableFilter
#' @export Filter<-
`Filter<-` <- `TableFilter<-`