#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include Var.R
#' @include Operator.R
#' @include Operand.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

setMethod(
  f = "Expr",
  signature = c("character", "character", "ANY"),
  definition = function(.Object, operator, operand, metricScope) {
    var <- Var(.Object)
    if (is(var, ".gaVar")) {
      GaExpr(.Object, operator, operand, metricScope)
    } else if (is(var, ".mcfVar")) {
      McfExpr(.Object, operator, operand)
    } else if (is(var, ".rtVar")) {
      RtExpr(.Object, operator, operand)
    }
  }
)

setMethod(
  f = "GaExpr",
  signature = c("character", "character", "ANY"),
  definition = function(.Object, operator, operand, metricScope) {
    var <- GaVar(.Object)
    if (class(var) == "gaDimVar") {
      operator <- GaDimOperator(operator)
      operand <- GaOperand(as.character(operand))
      gaExprClass <- "gaDimExpr"
      new(gaExprClass, var = var, operator = operator, operand = operand)
    } else if (class(var) == "gaMetVar") {
      operator <- GaMetOperator(operator)
      operand <- GaOperand(as.numeric(operand))
      if (metricScope != "") {
        gaExprClass <- "gaSegMetExpr"
        new(
          gaExprClass,
          var = var,
          operator = operator,
          operand = operand,
          metricScope = metricScope
        )
      } else {
        gaExprClass <- "gaMetExpr"
        new(gaExprClass, var = var, operator = operator, operand = operand)
      }
    } else {
      stop(paste("Unsupported .gaVar class", class(var), sep=": "))
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
      operand <- McfOperand(as.character(operand))
      exprClass <- "mcfDimExpr"
      new(exprClass, var = var, operator = operator, operand = operand)
    } else if (class(var) == "mcfMetVar") {
      operator <- McfMetOperator(operator)
      operand <- McfOperand(as.numeric(operand))
      exprClass <- "mcfMetExpr"
      new(exprClass, var = var, operator = operator, operand = operand)
    } else {
      stop(paste("Unsupported .mcfVar class", class(var), sep=": "))
    }
  }
)

setMethod(
  f = "RtExpr",
  signature = c("character", "character", "ANY"),
  definition = function(.Object, operator, operand) {
    var <- RtVar(.Object)
    if (class(var) == "rtDimVar") {
      operator <- RtDimOperator(operator)
      operand <- RtOperand(as.character(operand))
      exprClass <- "rtDimExpr"
      new(exprClass, var = var, operator = operator, operand = operand)
    } else if (class(var) == "rtMetVar") {
      operator <- RtMetOperator(operator)
      operand <- RtOperand(as.numeric(operand))
      exprClass <- "rtMetExpr"
      new(exprClass, var = var, operator = operator, operand = operand)
    } else {
      stop(paste("Unsupported .rtVar class", class(var), sep=": "))
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

