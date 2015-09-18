#' @include operand-classes.R
#' @include expression-classes.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include operand-coerce.R
#' @importFrom methods setMethod as
NULL

# ---- Operand ----

#' @describeIn Operand
setMethod("Operand", ".operand", function(object) {object})

#' @describeIn Operand
setMethod("Operand", ".expr", function(object) {as(object, ".operand")})

#' @describeIn Operand
setMethod(
  f = "Operand<-",
  signature = c(".expr", "ANY"),
  definition = function(object, value) {
    as(object, ".operand") <- value
    object
  }
)
