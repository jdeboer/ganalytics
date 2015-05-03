#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# ---- Operator, GaDimOperator, GaMetOperator ----

setMethod(
  f = "Operator",
  signature = ".operator",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "Operator<-",
  signature = c(".operator", "character"),
  definition = function(.Object, value) {
    as(.Object, "character") <- value
    return(.Object)
  }
)

setMethod(
  f = "Operator",
  signature = ".expr",
  definition = function(.Object) {
    Operator(.Object@operator)
  }
)

setMethod(
  f = "Operator<-",
  signature = ".expr",
  definition = function(.Object, value) {
    Operator(.Object@operator) <- value
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
    IsRegEx(Operator(.Object))
  }
)
