#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

setMethod(
  f = "GaAnd",
  signature = ".gaCompoundExpr",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    exprList <- lapply(
      X = exprList,
      FUN = function(expr) {
        if (is(expr, "gaAnd")) {
          expr <- unlist(expr, recursive = FALSE)
          expr <- lapply(
            X = expr,
            FUN = function(exprB) {
              as(exprB, "gaOr")
            }
          )
        } else {
          as(object = expr, Class = "gaOr")
        }
      }
    )
    x <- sapply(
      X = exprList,
      FUN = function(expr) {
        !is(expr, "gaOr")
      }
    )
    exprList <- c(
      exprList[!x],
      unlist(
        exprList[x],
        recursive = FALSE
      )
    )
    new("gaAnd", exprList)
  }
)

setMethod(
  f = "&",
  signature = c(".gaCompoundExpr", ".gaCompoundExpr"),
  definition = function(e1, e2) {
    GaAnd(e1, e2)
  }
)

