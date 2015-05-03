#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
NULL

setMethod(
  f = "TableFilter",
  signature = ".query",
  definition = function(.Object) {
    as(.Object, ".tableFilter")
  }
)

setMethod(
  f = "TableFilter",
  signature = "NULL",
  definition = function(.Object) {
    as(.Object, ".tableFilter")
  }
)

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

# setMethod(
#   f = "TableFilter",
#   signature = ".expr",
#   definition = function(.Object) {
#     as(.Object, ".tableFilter")
#   }
# )

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

setMethod(
  f = "TableFilter<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    as(.Object, ".tableFilter") <- value
    validObject(.Object)
    .Object
  }
)

# Backwards compatibility
#' @export GaFilter
GaFilter <- TableFilter
`GaFilter<-` <- `TableFilter<-`
