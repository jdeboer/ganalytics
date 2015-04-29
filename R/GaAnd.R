#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

setMethod(
  f = "GaAnd",
  signature = ".compoundExpr",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    exprList <- lapply(
      X = exprList,
      FUN = function(expr) {
        if (is(expr, "andExpr")) {
          expr <- unlist(expr, recursive = FALSE)
          expr <- lapply(
            X = expr,
            FUN = function(exprB) {
              as(exprB, "orExpr")
            }
          )
        } else {
          as(object = expr, Class = "orExpr")
        }
      }
    )
    x <- sapply(
      X = exprList,
      FUN = function(expr) {
        !is(expr, "orExpr")
      }
    )
    exprList <- c(
      exprList[!x],
      unlist(
        exprList[x],
        recursive = FALSE
      )
    )
    new("andExpr", exprList)
  }
)

setMethod(
  f = "&",
  signature = c(".compoundExpr", ".compoundExpr"),
  definition = function(e1, e2) {
    GaAnd(e1, e2)
  }
)

