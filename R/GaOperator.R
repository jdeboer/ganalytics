

# ---- GaOperator, GaDimOperator, GaMetOperator ----

setMethod(
  f = "GaOperator",
  signature = ".gaOperator",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaOperator<-",
  signature = c(".gaOperator", "character"),
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


# ---- GaOperator, GaDimOperator, GaMetOperator ----

setMethod(
  f = "GaOperator",
  signature = ".gaExpr",
  definition = function(.Object) {
    GaOperator(.Object@gaOperator)
  }
)

setMethod(
  f = "GaOperator<-",
  signature = ".gaExpr",
  definition = function(.Object, value) {
    GaOperator(.Object@gaOperator) <- value
    return(.Object)
  }
)

