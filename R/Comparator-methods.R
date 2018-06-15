#' @include comparator-classes.R
#' @include expr-classes.R
#' @include Comparator-generics.R
#' @include comparator-coerce.R
#' @include utils.R
#' @importFrom methods as
NULL

# ---- %matches%, %between%, %starts_with%, %in%, ==, !=, >, <, >=, <= ----

#' @rdname Comparator
setMethod(
  f = "%matches%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    Expr(var, "=~", operand)
  }
)

#' @rdname Comparator
setMethod(
  f = "%starts_with%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    Expr(var, "BEGINS_WITH", operand)
  }
)

#' @rdname Comparator
setMethod(
  f = "%ends_with%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    Expr(var, "ENDS_WITH", operand)
  }
)

#' @rdname Comparator
setMethod(
  f = "%contains%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    Expr(var, "=@", operand)
  }
)

#' @rdname Comparator
setMethod(
  f = "%between%",
  signature = c(".var", ".operand"),
  function(var, operand) {
    Expr(var, "<>", operand)
  }
)

#' @rdname Comparator
setMethod(
  f = "%in%",
  signature = c(".var", ".operand"),
  function(x, table) {
    Expr(x, "[]", table)
  }
)

#' @rdname Comparator
#' @param e1 Dimension or metric object
#' @param e2 Operand object
setMethod(
  f = "==",
  signature = c(".var", ".operand"),
  function(e1, e2) {
    Expr(e1, "==", e2)
  }
)

#' @rdname Comparator
setMethod(
  f = "!=",
  signature = c(".var", ".operand"),
  function(e1, e2) {
    Expr(e1, "!=", e2)
  }
)

#' @rdname Comparator
setMethod(
  f = ">",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    Expr(e1, ">", e2)
  }
)

#' @rdname Comparator
setMethod(
  f = "<",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    Expr(e1, "<", e2)
  }
)

#' @rdname Comparator
setMethod(
  f = ">=",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    Expr(e1, ">=", e2)
  }
)

#' @rdname Comparator
setMethod(
  f = "<=",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    Expr(e1, "<=", e2)
  }
)

# ---- Comparator, GaDimComparator, GaMetComparator ----

#' @describeIn Comparator Return the comparator used within the supplied
#'   conditional expression.
setMethod("Comparator", signature = ".expr", definition = function(object){
  object@comparator
})

#' @describeIn Comparator Replace the comparator of the supplied
#'   conditional expression.
setMethod("Comparator<-", signature = c(".expr", "ANY"), definition = function(object, value){
  use_class <- class(object@comparator)
  object@comparator <- as(value, use_class)
  validObject(object)
  object
})

#' @describeIn IsRegEx Test whether the supplied comparator is for a regular
#'   expression.
setMethod("IsRegEx", ".dimComparator", function(object) {
  object %in% c("=~", "!~")
})

#' @describeIn IsRegEx Test whether a conditional expression is using regular
#'   expression match.
setMethod("IsRegEx", ".expr", function(object) {
  IsRegEx(Comparator(object))
})
