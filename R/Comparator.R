#' @include comparator-classes.R
#' @include expression-classes.R
#' @include all-generics.R
#' @include all-coercions.R
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

#' @describeIn Comparator
setMethod("Comparator", ".comparator", function(object) {object})

#' @describeIn Comparator
setMethod(
  f = "Comparator<-",
  signature = c(".comparator", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

#' @describeIn Comparator
setMethod("Comparator", ".expr", function(object) {as(object, ".comparator")})

#' @describeIn Comparator
setMethod(
  f = "Comparator<-",
  signature = ".expr",
  definition = function(object, value) {
    as(object, ".comparator") <- value
    object
  }
)

# ---- IsRegEx ----
#' @describeIn IsRegEx
setMethod("IsRegEx", ".dimComparator", function(object) {object %in% c("=~", "!~")})

#' @describeIn IsRegEx
setMethod("IsRegEx", ".expr", function(object) {IsRegEx(Comparator(object))})
