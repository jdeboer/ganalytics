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
  f = "GaOperand",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "gaDimOperand")
  }
)

setMethod(
  f = "GaOperand",
  signature = "numeric",
  definition = function(.Object) {
    as(.Object, "gaMetOperand")
  }
)

setMethod(
  f = "McfOperand",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "mcfDimOperand")
  }
)

setMethod(
  f = "McfOperand",
  signature = "numeric",
  definition = function(.Object) {
    as(.Object, "mcfMetOperand")
  }
)

setMethod(
  f = "RtOperand",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "rtDimOperand")
  }
)

setMethod(
  f = "RtOperand",
  signature = "numeric",
  definition = function(.Object) {
    as(.Object, "rtMetOperand")
  }
)

setMethod(
  f = "Operand<-",
  signature = ".operand",
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

