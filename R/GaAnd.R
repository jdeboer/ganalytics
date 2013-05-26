#' @title GaAnd
#' @description methods for ANDing two or more ganalytics expressions together.
#' @details
#' Create a new AND expression from one or more arguments
#' Valid types are either AND, OR, or single expressions.
#' A single list of objects is also accepted.
#' 
#' @include classCoercion.R

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

