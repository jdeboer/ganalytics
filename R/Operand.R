#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# ---- Operand ----

#' @describeIn Expr
setMethod("Operand", ".operand", function(object) {object})

#' @describeIn Expr
setMethod("Operand", ".expr", function(object) {as(object, ".operand")})

#' @describeIn Expr
setMethod(
  f = "Operand<-",
  signature = c(".expr", "ANY"),
  definition = function(object, value) {
    as(object, ".operand") <- value
    object
  }
)
