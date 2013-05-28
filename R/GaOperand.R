#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

# ---- GaOperand ----

setMethod(
  f = "GaOperand",
  signature = ".gaOperand",
  definition = function(.Object) {
    return(.Object@.Data)
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
  signature = c(".gaOperand", "ANY"),
  definition = function(.Object, value) {
    to <- class(value)
    as(.Object, to) <- value
    return(.Object)
  }
)


# ---- GaOperand ----

setMethod(
  f = "GaOperand",
  signature = ".gaExpr",
  definition = function(.Object) {
    GaOperand(.Object@gaOperand)
  }
)

setMethod(
  f = "GaOperand<-",
  signature = ".gaExpr",
  definition = function(.Object, value) {
    GaOperand(.Object@gaOperand) <- value
    return(.Object)
  }
)

