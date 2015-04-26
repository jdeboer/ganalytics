#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# ---- GaOperator, GaDimOperator, GaMetOperator ----

setMethod(
  f = "GaOperator",
  signature = ".operator",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaOperator<-",
  signature = c(".operator", "character"),
  definition = function(.Object, value) {
    as(.Object, "character") <- value
    return(.Object)
  }
)

setMethod(
  f = "GaDimOperator",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "gaDimOperator")
  }
)

setMethod(
  f = "GaMetOperator",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "gaMetOperator")
  }
)


setMethod(
  f = "McfDimOperator",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "mcfDimOperator")
  }
)

setMethod(
  f = "McfMetOperator",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "mcfMetOperator")
  }
)

# ---- GaOperator, GaDimOperator, GaMetOperator ----

setMethod(
  f = "GaOperator",
  signature = ".expr",
  definition = function(.Object) {
    GaOperator(.Object@operator)
  }
)

setMethod(
  f = "GaOperator<-",
  signature = ".expr",
  definition = function(.Object, value) {
    GaOperator(.Object@operator) <- value
    return(.Object)
  }
)

# ---- IsRegEx ----

setMethod(
  f = "IsRegEx",
  signature = ".dimOperator",
  definition = function(.Object) {
    return(
      .Object %in% c("=~", "!~")
    )
  }
)

setMethod(
  f = "IsRegEx",
  signature = ".expr",
  definition = function(.Object) {
    IsRegEx(GaOperator(.Object))
  }
)