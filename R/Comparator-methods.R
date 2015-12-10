#' @include comparator-classes.R
#' @include expr-classes.R
#' @include Comparator-generics.R
#' @include comparator-coerce.R
#' @include utils.R
NULL

# ---- %matches%, %between%, %starts_with%, %in%, ==, !=, >, <, >=, <= ----

#' @rdname Comparator
setMethod(
  f = "%matches%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    comparator <- "=~"
    Expr(var, comparator, operand)
  }
)

#' @rdname Comparator
setMethod(
  f = "%starts_with%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    comparator <- "=@"
    Expr(var, comparator, operand)
  }
)

#' @rdname Comparator
setMethod(
  f = "%between%",
  signature = c(".var", ".operand"),
  function(var, operand) {
    comparator <- "<>"
    Expr(var, comparator, operand)
  }
)

#' @rdname Comparator
setMethod(
  f = "%in%",
  signature = c(".var", ".operand"),
  function(x, table) {
    comparator <- "[]"
    Expr(x, comparator, table)
  }
)

#' @rdname Comparator
#' @param e1 Dimension or metric object
#' @param e2 Operand object
setMethod(
  f = "==",
  signature = c(".var", ".operand"),
  function(e1, e2) {
    comparator <- "=="
    Expr(e1, comparator, e2)
  }
)

#' @rdname Comparator
setMethod(
  f = "!=",
  signature = c(".var", ".operand"),
  function(e1, e2) {
    comparator <- "!="
    Expr(e1, comparator, e2)
  }
)

#' @rdname Comparator
setMethod(
  f = ">",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    comparator <- ">"
    Expr(e1, comparator, e2)
  }
)

#' @rdname Comparator
setMethod(
  f = "<",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    comparator <- "<"
    Expr(e1, comparator, e2)
  }
)

#' @rdname Comparator
setMethod(
  f = ">=",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    comparator <- ">="
    Expr(e1, comparator, e2)
  }
)

#' @rdname Comparator
setMethod(
  f = "<=",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    comparator <- "<="
    Expr(e1, comparator, e2)
  }
)

# ---- Comparator, GaDimComparator, GaMetComparator ----

#' @describeIn Comparator Return the comparator used within the supplied
#'   conditional expression.
setMethod("Comparator", "ANY", function(object) {as(object, ".comparator")})

#' @describeIn Comparator Replace the comparator used in the supplied
#'   conditional expression.
setMethod(
  f = "Comparator<-",
  signature = c("ANY", "ANY"),
  definition = function(object, value) {
    as(object, ".comparator") <- as(value, ".comparator")
    object
  }
)

#' @describeIn IsRegEx Test whether the supplied comparator is for a regular
#'   expression.
setMethod("IsRegEx", ".dimComparator", function(object) {
  object %in% c("=~", "!~")
})

#' @describeIn IsRegEx Test whether a conditional epxression is using regular
#'   expression match.
setMethod("IsRegEx", ".expr", function(object) {
  IsRegEx(Comparator(object))
})
