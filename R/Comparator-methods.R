#' @include comparator-classes.R
#' @include expr-classes.R
#' @include Comparator-generics.R
#' @include comparator-coerce.R
#' @include utils.R
#' @importFrom methods as callNextMethod
NULL

setMethod(
  f = "initialize",
  signature = ".comparator",
  definition = function(.Object, value, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!missing(value)) {
      value <- toupper(value)
      if (value %in% c("=", "EXACT", "EQUAL")) value <- "=="
      else if (value %in% c("][", "IN_LIST")) value <- "[]"
      else if (value %in% c("><", "BETWEEN", "NUMERIC_BETWEEN")) value <- "<>"
      else if (value %in% c("<<", "LESS_THAN", "NUMERIC_LESS_THAN")) value <- "<"
      else if (value %in% c(">>", "GREATER_THAN", "NUMERIC_GREATER_THAN")) value <- ">"
      else if (value %in% c("~=", "~", "MATCHES", "REGEXP")) value <- "=~"
      else if (value %in% c("@=", "@", "CONTAINS", "PARTIAL")) value <- "=@"
      else if (value %in% c("!","=!")) value <- "!="
      else if (value == "=>") value <- ">="
      else if (value == "=<") value <- "<="
      else if (value == "~!") value <- "!~"
      else if (value == "@!") value <- "!@"
      if(inherits(.Object, ".gaComparator")) {
        if (value %in% c(
          names(kGa4Ops$metric_operators),
          names(kGa4Ops$dimension_operators)
        )) {
          .Object@operator <- value
          value <- c(
            kGa4Ops$metric_operators, kGa4Ops$dimension_operators
          )[value]
        } else {
          if(value %in% kGa4Ops$negated_operators) {
            non_negated_value <- names(which(value == kGa4Ops$negated_operators))
          } else {
            non_negated_value <- value
          }
          if (inherits(.Object, ".dimComparator")) {
            .Object@operator <- names(which(non_negated_value == kGa4Ops$dimension_operators))
          } else {
            .Object@operator <- names(which(non_negated_value == kGa4Ops$metric_operators))
          }
        }
        .Object@negated <- value %in% kGa4Ops$negated_operators
      }
      .Object@.Data <- value
      validObject(.Object)
    }
    .Object
  }
)

# ---- %matches%, %between%, %starts_with%, %in%, ==, !=, >, <, >=, <= ----

#' Does the value of the dimension on the left match the regular expression on
#' the right.
#' @rdname grapes-matches-grapes
setMethod(
  f = "%matches%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    Expr(var, "=~", operand)
  }
)

#' Does the value of the dimension on the left start with the character
#' substring on the right.
#' @rdname grapes-starts_with-grapes
setMethod(
  f = "%matches%",
  signature = c(".var", "character"),
  function(var, operand) {
    Expr(var, "=~", operand)
  }
)

#' @rdname comparators
setMethod(
  f = "%starts_with%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    Expr(var, "BEGINS_WITH", operand)
  }
)

#' Does the value of the dimension on the left end with the character substring
#' on the right.
#' @rdname grapes-ends_with-grapes
setMethod(
  f = "%starts_with%",
  signature = c(".var", "character"),
  function(var, operand) {
    Expr(var, "BEGINS_WITH", operand)
  }
)

#' @rdname comparators
setMethod(
  f = "%ends_with%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    Expr(var, "ENDS_WITH", operand)
  }
)

#' Does the value of the dimension on the left contain the character substring
#' on the right.
#' @rdname grapes-contains-grapes
setMethod(
  f = "%ends_with%",
  signature = c(".var", "character"),
  function(var, operand) {
    Expr(var, "ENDS_WITH", operand)
  }
)

#' @rdname comparators
setMethod(
  f = "%contains%",
  signature = c(".var", ".dimOperand"),
  function(var, operand) {
    Expr(var, "=@", operand)
  }
)

#' Is the value on the left in between the values of the first and second
#' elements of the vector on the right.
#' @rdname grapes-between-grapes
setMethod(
  f = "%contains%",
  signature = c(".var", "character"),
  function(var, operand) {
    Expr(var, "=@", operand)
  }
)

#' @rdname comparators
setMethod(
  f = "%between%",
  signature = c(".var", ".operand"),
  function(var, operand) {
    Expr(var, "<>", operand)
  }
)

#' Does the vector on the right contain an element that matches the value on the
#' left.
#' @rdname grapes-in-grapes
setMethod(
  f = "%in%",
  signature = c(".var", ".operand"),
  function(x, table) {
    Expr(x, "[]", table)
  }
)

#' Equals to
#'
#' Do the values on the left and right match exactly.
#'
#' @param e1 Dimension or metric object
#' @param e2 Operand object
#'
#' @family comparator_functions
#'
#' @rdname equals
setMethod(
  f = "==",
  signature = c(".var", ".operand"),
  function(e1, e2) {
    Expr(e1, "==", e2)
  }
)

#' Not equals to
#'
#' Expr Do the values on the left and right not match.
#'
#' @param e1 Dimension or metric object
#' @param e2 Operand object
#'
#' @family comparator_functions
#'
#' @name not-equals
NULL

#' @rdname not-equals
setMethod(
  f = "!=",
  signature = c(".var", ".operand"),
  function(e1, e2) {
    Expr(e1, "!=", e2)
  }
)

#' Greater than
#'
#' Is the value on the left greater than the value on the right.
#'
#' @param e1 Dimension or metric object
#' @param e2 Operand object
#'
#' @family comparator_functions
#'
#' @rdname greater-than
setMethod(
  f = ">",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    Expr(e1, ">", e2)
  }
)

#' Less than
#'
setMethod(
  f = ">",
  signature = c(".var", "numeric"),
  function(e1, e2) {
    Expr(e1, ">", e2)
  }
)

setMethod(
  f = ">",
  signature = c(".var", "integer"),
  function(e1, e2) {
    Expr(e1, ">", e2)
  }
)

#' Is the value on the left less than the value on the right.
#'
#' @param e1 Dimension or metric object
#' @param e2 Operand object
#'
#' @family comparator_functions
#'
#' @rdname less-than
setMethod(
  f = "<",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    Expr(e1, "<", e2)
  }
)

#' Greater or equals to
#'
setMethod(
  f = "<",
  signature = c(".var", "numeric"),
  function(e1, e2) {
    Expr(e1, "<", e2)
  }
)

setMethod(
  f = "<",
  signature = c(".var", "integer"),
  function(e1, e2) {
    Expr(e1, "<", e2)
  }
)

#' Is the value on the left greater than or equal to the value on the right.
#'
#' @param e1 Dimension or metric object
#' @param e2 Operand object
#'
#' @family comparator_functions
#'
#' @rdname greater-than-or-equal-to
setMethod(
  f = ">=",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    Expr(e1, ">=", e2)
  }
)

#' Less than or equals to
#'
setMethod(
  f = ">=",
  signature = c(".var", "numeric"),
  function(e1, e2) {
    Expr(e1, ">=", e2)
  }
)

setMethod(
  f = ">=",
  signature = c(".var", "integer"),
  function(e1, e2) {
    Expr(e1, ">=", e2)
  }
)

#' Is the value on the left less than or equal to the value on the right.
#'
#' @param e1 Dimension or metric object
#' @param e2 Operand object
#'
#' @family comparator_functions
#'
#' @rdname less-than-or-equal-to
setMethod(
  f = "<=",
  signature = c(".var", ".metOperand"),
  function(e1, e2) {
    Expr(e1, "<=", e2)
  }
)

setMethod(
  f = "<=",
  signature = c(".var", "numeric"),
  function(e1, e2) {
    Expr(e1, "<=", e2)
  }
)

setMethod(
  f = "<=",
  signature = c(".var", "integer"),
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
