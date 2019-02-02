#' @include expr-classes.R
#' @include Expr-generics.R
#' @include expr-coerce.R
#' @include Var-methods.R
#' @include Comparator-methods.R
#' @include Operand-methods.R
#' @include utils.R
#' @importFrom methods setMethod new validObject as callNextMethod
#' @importFrom assertthat assert_that
NULL

setMethod(
  f = "initialize",
  signature = ".dimExpr",
  definition = function(.Object, var, comparator, operand, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@var <- var
    .Object@comparator <- comparator
    var <- as.character(var)
    if (comparator %in% c("!=", "==", "[]", "<>")) {
      if (var %in% kGaDimTypes$bools) {
        operand <- as(as(operand, "logical"), class(operand))
      } else if (var %in% c("ga:visitorType", "ga:userType")) {
        visitorType <- c("New Visitor", "Returning Visitor")
        index <- pmatch(x = tolower(operand), table = tolower(visitorType))
        if (is.na(index)) {
          stop(paste(var, "Invalid operand", operand, sep = ": "))
        } else {
          operand <- as(visitorType[index], class(operand))
        }
      } else if (var == "ga:date") {
        operand <- as(format(ymd(operand), format = "%Y%m%d"), class(operand))
      } else if (var == "dateOfSession") {
        operand <- as(format(ymd(operand), format = "%Y-%m-%d"), class(operand))
      }
    }
    if (IsRegEx(.Object)) {
      operand <- tolower(operand)
    }
    Operand(.Object) <- operand
    validObject(.Object)
    .Object
  }
)

#' @describeIn Expr Returns itself.
setMethod("Expr", ".expr", function(object) {object})

#' @describeIn Expr Use non-standard formula evaluation to define an expression.
#'   Accepts a formula in the form of: \code{~ <variable> <comparator>
#'   <operand>} where only the \code{<operand>} is evaluated.
#' @examples
#' source_google <- Expr(~source == "google")
setMethod(
  f = "Expr",
  signature = c("formula", "ANY"),
  definition = function(object, metricScope) {
    object <- as(object, ".expr")
    if(!missing(metricScope) && metricScope != "") {
      ScopeLevel(object) <- metricScope
    }
    object
  }
)

#' @describeIn Expr Return an expression composed of the supplied variable,
#'   comparator and operand arguments.
#' @examples
#' source_google <- Expr("source", "==", "google")
#' bounces <- Expr("bounces", ">", 0)
setMethod(
  f = "Expr",
  signature = c("character", "character", "ANY", "ANY"),
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

#' @describeIn GaExpr Return a Google Analytics expression using the supplied
#'   variable, operator and operand.
#'   bounces <- GaExpr("bounces", ">", 0)
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
      if (metricScope %in% c("perUser", "perSession", "perHit", "perProduct")) {
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

#' @describeIn McfExpr Return a Multi-channel Funnel condition composed of the
#'   supplied arguments describing the variable, comparator and operator.
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

#' @describeIn RtExpr Define a Real-Time Reporting condition using the arguments
#'   describing the variable, comparator and operand.
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

#' @describeIn ScopeLevel Return the scope of the supplied metric used within a
#'   segment definition.
setMethod(
  "ScopeLevel",
  signature = c("gaSegMetExpr", "missing"),
  definition = function(object) {object@metricScope}
)

#' @describeIn ScopeLevel Set the scope, as described by a character value, to
#'   be applied to the supplied metric condition for use within a segment
#'   expression.
setMethod(
  "ScopeLevel",
  signature = c("gaSegMetExpr", "character"),
  definition = function(object, value) {
    object@metricScope <- value
    validObject(object)
    object
  }
)

#' @describeIn ScopeLevel Set the scope, as described by a character value, to
#'   be applied to the supplied metric condition for use within a segment
#'   expression.
setMethod(
  f = "ScopeLevel<-",
  signature = c("gaMetExpr", "character"),
  definition = function(object, value) {
    object <- as(object, "gaSegMetExpr")
    object@metricScope <- value
    object
  }
)
