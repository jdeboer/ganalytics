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
