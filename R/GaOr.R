#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# Create a GaOr from one or more metric or dimension expressions
# Takes one or more Metric or Dimension expressions
# as separate arguments or as a list.
# Returns an object of gaOr.

setMethod(
  f = "GaOr",
  signature = ".gaCompoundExpr",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    exprList <- lapply(
      X = exprList,
      FUN = function(expr) {
        as(object = expr, Class = "gaOr")
      }
    )
    as(new("gaAnd", exprList), "gaOr")
  }
)

setMethod(
  f = "|",
  signature = c(".gaCompoundExpr", ".gaCompoundExpr"),
  definition = function(e1, e2) {
    GaOr(e1, e2)
  }
)
