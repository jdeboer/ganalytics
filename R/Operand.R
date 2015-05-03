#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# ---- Operand ----

setMethod("Operand", ".operand", function(.Object) {.Object})

setMethod("Operand", ".expr", function(.Object) {as(.Object, ".operand")})

setMethod(
  f = "Operand<-",
  signature = ".expr",
  definition = function(.Object, value) {
    as(.Object, ".operand") <- value
    .Object
  }
)
