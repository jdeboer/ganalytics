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
  f = "GaFilter",
  signature = "gaDynSegment",
  definition = function(.Object) {
    as(.Object, ".tableFilter")
  }
)

setMethod(
  f = "GaFilter",
  signature = "NULL",
  definition = function(.Object) {
    new("gaFilter", list())
  }
)

setMethod(
  f = "McfFilter",
  signature = "NULL",
  definition = function(.Object) {
    new("mcfFilter", list())
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
  f = "RtFilter",
  signature = "NULL",
  definition = function(.Object) {
    new("rtFilter", list())
  }
)

setMethod(
  f = "GaFilter<-",
  signature = c("gaFilter", "andExpr"),
  definition = function(.Object, value) {
    as(.Object, "andExpr") <- value
    return(.Object)
  }
)

# Backwards compatibility
#' @export GaFilter
GaFilter <- TableFilter
`GaFilter<-` <- `TableFilter<-`

setMethod(
  f = "GaFilter<-",
  signature = c("gaFilter", "ANY"),
  definition = function(.Object, value) {
    as(value, "gaFilter")
  }
)


setMethod(
  f = "GaFilter",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@filters
  }
)

setMethod(
  f = "GaFilter<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@filters <- GaFilter(value)
    validObject(.Object)
    return(.Object)
  }
)

