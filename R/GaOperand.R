#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# ---- GaOperand ----

setMethod(
  f = "GaOperand",
  signature = ".gaOperand",
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
  f = "GaOperand<-",
  signature = ".gaOperand",
  definition = function(.Object, value) {
    to <- class(value)
    as(.Object, to) <- value
    return(.Object)
  }
)

# ---- GaOperand ----

setMethod(
  f = "GaOperand",
  signature = ".expr",
  definition = function(.Object) {
    GaOperand(.Object@operand)
  }
)

setMethod(
  f = "GaOperand<-",
  signature = ".expr",
  definition = function(.Object, value) {
    GaOperand(.Object@operand) <- value
    return(.Object)
  }
)

