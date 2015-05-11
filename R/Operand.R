#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
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
