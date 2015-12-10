#' @include operand-classes.R
#' @include expr-classes.R
#' @include Operand-generics.R
#' @include operand-coerce.R
#' @importFrom methods setMethod as
NULL

# ---- Operand ----

#' @describeIn Operand Return the operand used within the condition, or coerce
#'   the supplied value into an operand.
setMethod("Operand", "ANY", function(object) {as(object, ".operand")})

#' @describeIn Operand Replace the operand of a condition.
setMethod(
  f = "Operand<-",
  signature = c(".expr", "ANY"),
  definition = function(object, value) {
    as(object, ".operand") <- value
    object
  }
)
