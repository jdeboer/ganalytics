#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# Create an Or from one or more metric or dimension expressions
# Takes one or more Metric or Dimension expressions
# as separate arguments or as a list.
# Returns an object of gaOr.

setMethod(
  f = "Or",
  signature = ".compoundExpr",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    exprList <- lapply(
      X = exprList,
      FUN = function(expr) {
        assert_that(!is(expr, "andExpr") | length(exprList) == 1)
        expr <- as(expr, "orExpr")
      }
    )
    exprList <- unlist(exprList, recursive = FALSE)
    new("orExpr", exprList)    
  }
)

setMethod(
  f = "|",
  signature = c(".compoundExpr", ".compoundExpr"),
  definition = function(e1, e2) {
    Or(e1, e2)
  }
)

# Backwards compatibility
#' @export GaOr
GaOr <- Or
