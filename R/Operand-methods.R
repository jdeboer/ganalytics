#' @include operand-classes.R
#' @include expr-classes.R
#' @include Operand-generics.R
#' @include operand-coerce.R
#' @importFrom methods setMethod as
NULL

# ---- Operand ----

#' @describeIn Operand Returns itself.
setMethod("Operand", ".operand", function(object) {object})

#' @describeIn Operand Return the operand used within the condition.
setMethod("Operand", ".expr", function(object) {as(object, ".operand")})

#' @describeIn Operand Coerce a character value as dimension operand.
setMethod("Operand", "character", function(object) {as(object, ".operand")})

#' @describeIn Operand Coerce a numeric value as a metric operand.
setMethod("Operand", "numeric", function(object) {as(object, ".operand")})

#' @describeIn Operand Coerce a logical value as a dimension operator.
setMethod("Operand", "logical", function(object) {as(object, ".operand")})

#' @describeIn Operand Replace the operand of a condition.
setMethod(
  f = "Operand<-",
  signature = c(".expr", "ANY"),
  definition = function(object, value) {
    as(object, ".operand") <- value
    object
  }
)
