#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include GaVar.R
#' @include GaOperator.R
#' @include GaOperand.R
NULL

setMethod(
  f = "GaExpr",
  signature = c("character", "character", "ANY"),
  definition = function(.Object, gaOperator, gaOperand, metricScope) {
    gaVar <- GaVar(.Object)
    if (class(gaVar) == "gaDimVar") {
      gaOperator <- GaDimOperator(gaOperator)
      gaOperand <- GaOperand(as.character(gaOperand))
      gaExprClass <- "gaDimExpr"
      new(
        Class = gaExprClass,
        gaVar = gaVar,
        gaOperator = gaOperator,
        gaOperand = gaOperand
      )
    } else if (class(gaVar) == "gaMetVar") {
      gaOperator <- GaMetOperator(gaOperator)
      gaOperand <- GaOperand(as.numeric(gaOperand))
      gaExprClass <- "gaMetExpr"
      new(
        Class = gaExprClass,
        gaVar = gaVar,
        gaOperator = gaOperator,
        gaOperand = gaOperand,
        metricScope = metricScope
      )
    } else {
      stop(
        paste("Unsupported .gaVar class", class(gaVar), sep=": ")
      )
    }
  }
)

