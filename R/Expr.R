#' @include expression-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include expr-coerce.R
#' @include Var.R
#' @include Comparator.R
#' @include Operand.R
#' @include utils.R
#' @importFrom methods setMethod new validObject
#' @importFrom lazyeval as.lazy
#' @importFrom assertthat assert_that
NULL

#' @describeIn Expr
#' Accepts a formula in the form of: \code{~ <variable> <comparator>
#'   <operand>} where only the \code{<operand>} is evaluated.
setMethod("Expr", ".expr", function(object) {object})

#' @describeIn Expr
setMethod(
  f = "Expr",
  signature = c("formula"),
  definition = function(object) {
    lazy_expr <- as.lazy(object)
    assert_that(length(lazy_expr$expr) == 3)
    comparator <- as.character(lazy_expr$expr[[1]])
    comparator <- switch(
      comparator,
      `%starts_with%` = "=@",
      `=@` = "=@",
      `%matches%` = "=~",
      `%=~%` = "=~",
      `%in%` = "[]",
      `%[]%` = "[]",
      `%between%` = "<>",
      `%<>%` = "<>",
      comparator
    )
    var <- as.character(lazy_expr$expr[[2]])
    operand <- as.expression(lazy_expr$expr[[3]])
    Expr(var, comparator, eval(operand, envir = lazy_expr$env))
  }
)

#' @describeIn Expr
setMethod(
  f = "Expr",
  signature = c("character", "character", "ANY"),
  definition = function(object, comparator, operand, metricScope) {
    var <- Var(object)
    if (is(var, ".gaVar")) {
      GaExpr(object, comparator, operand, metricScope)
    } else if (is(var, ".mcfVar")) {
      McfExpr(object, comparator, operand)
    } else if (is(var, ".rtVar")) {
      RtExpr(object, comparator, operand)
    } else stop("Variable type not recognised for expressions.")
  }
)

#' @describeIn GaExpr
setMethod(
  f = "GaExpr",
  signature = c("character", "character", "ANY"),
  definition = function(object, comparator, operand, metricScope) {
    var <- GaVar(object)
    if (class(var) == "gaDimVar") {
      comparator <- as(comparator, "gaDimComparator")
      operand <- as(operand, "gaDimOperand")
      gaExprClass <- "gaDimExpr"
      new(gaExprClass, var = var, comparator = comparator, operand = operand)
    } else if (class(var) == "gaMetVar") {
      comparator <- as(comparator, "gaMetComparator")
      operand <- as(operand, "gaMetOperand")
      if (metricScope != "") {
        gaExprClass <- "gaSegMetExpr"
        new(
          gaExprClass,
          var = var,
          comparator = comparator,
          operand = operand,
          metricScope = metricScope
        )
      } else {
        gaExprClass <- "gaMetExpr"
        new(gaExprClass, var = var, comparator = comparator, operand = operand)
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
  definition = function(object, comparator, operand) {
    var <- McfVar(object)
    if (class(var) == "mcfDimVar") {
      comparator <- as(comparator, "mcfDimComparator")
      operand <- as(operand, "mcfDimOperand")
      exprClass <- "mcfDimExpr"
      new(exprClass, var = var, comparator = comparator, operand = operand)
    } else if (class(var) == "mcfMetVar") {
      comparator <- as(comparator, "mcfMetComparator")
      operand <- as(operand, "mcfMetOperand")
      exprClass <- "mcfMetExpr"
      new(exprClass, var = var, comparator = comparator, operand = operand)
    } else {
      stop(paste("Unsupported .mcfVar class", class(var), sep = ": "))
    }
  }
)

#' @describeIn RtExpr
setMethod(
  f = "RtExpr",
  signature = c("character", "character", "ANY"),
  definition = function(object, comparator, operand) {
    var <- RtVar(object)
    if (class(var) == "rtDimVar") {
      comparator <- as(comparator, "rtDimComparator")
      operand <- as(operand, "rtDimOperand")
      exprClass <- "rtDimExpr"
      new(exprClass, var = var, comparator = comparator, operand = operand)
    } else if (class(var) == "rtMetVar") {
      comparator <- as(comparator, "rtMetComparator")
      operand <- as(operand, "rtMetOperand")
      exprClass <- "rtMetExpr"
      new(exprClass, var = var, comparator = comparator, operand = operand)
    } else {
      stop(paste("Unsupported .rtVar class", class(var), sep = ": "))
    }
  }
)

# ---- ScopeLevel, ScopeLevel<- ----

#' @describeIn ScopeLevel
setMethod("ScopeLevel", "gaSegMetExpr", function(object) {object@metricScope})

#' @describeIn ScopeLevel
setMethod(
  f = "ScopeLevel<-",
  signature = c("gaSegMetExpr", "character"),
  definition = function(object, value) {
    object@metricScope <- value
    validObject(object)
    object
  }
)

#' @describeIn ScopeLevel
setMethod(
  f = "ScopeLevel<-",
  signature = c("gaMetExpr", "character"),
  definition = function(object, value) {
    object <- as(object, "gaSegMetExpr")
    object@metricScope <- value
    validObject(object)
    object
  }
)
