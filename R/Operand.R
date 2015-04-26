#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# ---- Operand ----

setMethod(
  f = "Operand",
  signature = ".operand",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "Operand",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "gaDimOperand")
  }
)

setMethod(
  f = "Operand",
  signature = "numeric",
  definition = function(.Object) {
    as(.Object, "gaMetOperand")
  }
)

setMethod(
  f = "Operand<-",
  signature = ".gaOperand",
  definition = function(.Object, value) {
    to <- class(value)
    as(.Object, to) <- value
    return(.Object)
  }
)

# ---- Operand ----

setMethod(
  f = "Operand",
  signature = ".expr",
  definition = function(.Object) {
    Operand(.Object@operand)
  }
)

setMethod(
  f = "Operand<-",
  signature = ".expr",
  definition = function(.Object, value) {
    Operand(.Object@operand) <- value
    return(.Object)
  }
)

