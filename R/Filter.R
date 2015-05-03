#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
NULL

setMethod(
  f = "TableFilter",
  signature = ".tableFilter",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "TableFilter",
  signature = ".compoundExpr",
  definition = function(.Object) {
    as(.Object, ".tableFilter")
  }
)

setMethod(
  f = "TableFilter",
  signature = ".expr",
  definition = function(.Object) {
    as(.Object, ".tableFilter")
  }
)

setMethod(
  f = "TableFilter",
  signature = "gaDynSegment",
  definition = function(.Object) {
    as(.Object, ".tableFilter")
  }
)

setMethod(
  f = "TableFilter<-",
  signature = c(".tableFilter", "andExpr"),
  definition = function(.Object, value) {
    as(.Object, "andExpr") <- value
    .Object
  }
)

# setMethod(
#   f = "TableFilter<-",
#   signature = c(".tableFilter", "ANY"),
#   definition = function(.Object, value) {
#     as(value, ".tableFilter")
#   }
# )

setMethod(
  f = "TableFilter",
  signature = ".query",
  definition = function(.Object) {
    .Object@filters
  }
)

setMethod(
  f = "TableFilter<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    .Object@filters <- TableFilter(value)
    validObject(.Object)
    return(.Object)
  }
)

# Backwards compatibility
#' @export GaFilter
GaFilter <- TableFilter
`GaFilter<-` <- `TableFilter<-`
