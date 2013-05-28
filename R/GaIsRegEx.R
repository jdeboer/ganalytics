#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

# ---- GaIsRegEx ----

setMethod(
  f = "GaIsRegEx",
  signature = "gaDimOperator",
  definition = function(.Object) {
    return(
      .Object %in% c("=~", "!~")
    )
  }
)

setMethod(
  f = "GaIsRegEx",
  signature = ".gaExpr",
  definition = function(.Object) {
    GaIsRegEx(GaOperator(.Object))
  }
)
