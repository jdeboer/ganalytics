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

#' @describeIn Expr
setMethod("Expr", ".expr", function(object) {object})

#' @describeIn Expr
setMethod(
  f = "Expr",
  signature = c("character", "character", "ANY"),
  definition = function(object, operator, operand, metricScope) {
    var <- Var(object)
    if (is(var, ".gaVar")) {
      GaExpr(object, operator, operand, metricScope)
    } else if (is(var, ".mcfVar")) {
      McfExpr(object, operator, operand)
    } else if (is(var, ".rtVar")) {
      RtExpr(object, operator, operand)
    } else stop("Variable type not recognised for expressions.")
  }
)

#' @describeIn GaExpr
setMethod(
  f = "GaExpr",
  signature = c("character", "character", "ANY"),
  definition = function(object, operator, operand, metricScope) {
    var <- GaVar(object)
    if (class(var) == "gaDimVar") {
      operator <- as(operator, "gaDimOperator")
      operand <- as(operand, "gaDimOperand")
      gaExprClass <- "gaDimExpr"
      new(gaExprClass, var = var, operator = operator, operand = operand)
    } else if (class(var) == "gaMetVar") {
      operator <- as(operator, "gaMetOperator")
      operand <- as(operand, "gaMetOperand")
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
      stop(paste("Unsupported .gaVar class", class(var), sep = ": "))
    }
  }
)

#' @describeIn McfExpr
setMethod(
  f = "McfExpr",
  signature = c("character", "character", "ANY"),
  definition = function(object, operator, operand) {
    var <- McfVar(object)
    if (class(var) == "mcfDimVar") {
      operator <- as(operator, "mcfDimOperator")
      operand <- as(operand, "mcfDimOperand")
      exprClass <- "mcfDimExpr"
      new(exprClass, var = var, operator = operator, operand = operand)
    } else if (class(var) == "mcfMetVar") {
      operator <- as(operator, "mcfMetOperator")
      operand <- as(operand, "mcfMetOperand")
      exprClass <- "mcfMetExpr"
      new(exprClass, var = var, operator = operator, operand = operand)
    } else {
      stop(paste("Unsupported .mcfVar class", class(var), sep = ": "))
    }
  }
)

#' @describeIn RtExpr
setMethod(
  f = "RtExpr",
  signature = c("character", "character", "ANY"),
  definition = function(object, operator, operand) {
    var <- RtVar(object)
    if (class(var) == "rtDimVar") {
      operator <- as(operator, "rtDimOperator")
      operand <- as(operand, "rtDimOperand")
      exprClass <- "rtDimExpr"
      new(exprClass, var = var, operator = operator, operand = operand)
    } else if (class(var) == "rtMetVar") {
      operator <- as(operator, "rtMetOperator")
      operand <- as(operand, "rtMetOperand")
      exprClass <- "rtMetExpr"
      new(exprClass, var = var, operator = operator, operand = operand)
    } else {
      stop(paste("Unsupported .rtVar class", class(var), sep = ": "))
    }
  }
)

# ---- GaScopeLevel, GaScopeLevel<- ----

#' @describeIn GaScopeLevel
setMethod("GaScopeLevel", "gaSegMetExpr", function(object) {object@metricScope})

#' @describeIn GaScopeLevel
setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaSegMetExpr", "character"),
  definition = function(object, value) {
    object@metricScope <- value
    validObject(object)
    object
  }
)

#' @describeIn GaScopeLevel
setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaMetExpr", "character"),
  definition = function(object, value) {
    object <- as(object, "gaSegMetScope")
    object@metricScope <- value
    validObject(object)
    object
  }
)

#' @describeIn GaScopeLevel
setMethod(
  f = "GaScopeLevel",
  signature = "gaSegmentCondition",
  definition = function(object) {
    object@conditionScope
  }
)

#' @describeIn GaScopeLevel
setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaSegmentCondition", "character"),
  definition = function(object, value) {
    object@conditionScope <- value
    validObject(object)
    object
  }
)

