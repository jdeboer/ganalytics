#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include GaVar.R
#' @include GaOperator.R
#' @include Operand.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

setMethod(
  f = "GaExpr",
  signature = c("character", "character", "ANY"),
  definition = function(.Object, operator, operand, metricScope) {
    var <- GaVar(.Object)
    if (class(var) == "gaDimVar") {
      operator <- GaDimOperator(operator)
      operand <- Operand(as.character(operand))
      gaExprClass <- "gaDimExpr"
      new(
        Class = gaExprClass,
        var = var,
        operator = operator,
        operand = operand
      )
    } else if (class(var) == "gaMetVar") {
      operator <- GaMetOperator(operator)
      operand <- Operand(as.numeric(operand))
      if (metricScope != "") {
        gaExprClass <- "gaSegMetExpr"
        new(
          Class = gaExprClass,
          var = var,
          operator = operator,
          operand = operand,
          metricScope = metricScope
        )
      } else {
        gaExprClass <- "gaMetExpr"
        new(
          Class = gaExprClass,
          var = var,
          operator = operator,
          operand = operand
        )
      }
    } else {
      stop(
        paste("Unsupported .gaVar class", class(var), sep=": ")
      )
    }
  }
)

setMethod(
  f = "McfExpr",
  signature = c("character", "character", "ANY"),
  definition = function(.Object, operator, operand) {
    var <- McfVar(.Object)
    if (class(var) == "mcfDimVar") {
      operator <- McfDimOperator(operator)
      operand <- Operand(as.character(operand))
      exprClass <- "mcfDimExpr"
      new(
        Class = exprClass,
        var = var,
        operator = operator,
        operand = operand
      )
    } else if (class(var) == "mcfMetVar") {
      operator <- McfMetOperator(operator)
      operand <- Operand(as.numeric(operand))
      exprClass <- "mcfMetExpr"
      new(
        Class = exprClass,
        var = var,
        operator = operator,
        operand = operand
      )
    } else {
      stop(
        paste("Unsupported .mcfVar class", class(var), sep=": ")
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