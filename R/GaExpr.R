#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include GaVar.R
#' @include GaOperator.R
#' @include GaOperand.R
#' @include ganalytics-package.R
#' @include helper-functions.R
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
      if (metricScope != "") {
        gaExprClass <- "gaSegMetExpr"
        new(
          Class = gaExprClass,
          gaVar = gaVar,
          gaOperator = gaOperator,
          gaOperand = gaOperand,
          metricScope = metricScope
        )
      } else {
        gaExprClass <- "gaMetExpr"
        new(
          Class = gaExprClass,
          gaVar = gaVar,
          gaOperator = gaOperator,
          gaOperand = gaOperand
        )
      }
    } else {
      stop(
        paste("Unsupported .gaVar class", class(gaVar), sep=": ")
      )
    }
  }
)

# ---- GaScopeLevel, GaScopeLevel<- ----

setMethod(
  f = "GaScopeLevel",
  signature = "gaSegMetExpr",
  definition = function(.Object) {
    .Object@metricScope
  }
)

setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaSegMetExpr", "character"),
  definition = function(.Object, value) {
    .Object@metricScope <- value
    .Object
  }
)

setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaMetExpr", "character"),
  definition = function(.Object, value) {
    .Object <- as(.Object, "gaSegMetScope")
    .Object@metricScope <- value
    .Object
  }
)

setMethod(
  f = "GaScopeLevel",
  signature = "gaSegmentCondition",
  definition = function(.Object) {
    .Object@conditionScope
  }
)

setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaSegmentCondition", "character"),
  definition = function(.Object, value) {
    .Object@conditionScope <- value
    .Object
  }
)