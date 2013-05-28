#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

# ---- GaNot ----

setMethod(
  f = "GaNot",
  signature = ".gaOperator",
  definition = function(.Object) {
    if (.Object == "==") {
      GaOperator(.Object) <- "!="
      return(.Object)
    } else if (.Object == "!=") {
      GaOperator(.Object) <- "=="
      return(.Object)
    } else if (.Object == "<") {
      GaOperator(.Object) <- ">="
      return(.Object)
    } else if (.Object == ">=") {
      GaOperator(.Object) <- "<"
      return(.Object)
    } else if (.Object == ">") {
      GaOperator(.Object) <- "<="
      return(.Object)
    } else if (.Object == "<=") {
      GaOperator(.Object) <- ">"
      return(.Object)
    } else if (.Object == "!~") {
      GaOperator(.Object) <- "=~"
      return(.Object)
    } else if (.Object == "=~") {
      GaOperator(.Object) <- "!~"
      return(.Object)
    } else if (.Object == "=@") {
      GaOperator(.Object) <- "!@"
      return(.Object)
    } else if (.Object == "!@") {
      GaOperator(.Object) <- "=@"
      return(.Object)
    }
  }
)

setMethod(
  f = "GaNot",
  signature = ".gaExpr",
  definition = function(.Object) {
    GaOperator(.Object) <- GaNot(GaOperator(.Object))
    return(.Object)
  }
)

setMethod(
  f = "GaNot",
  signature = "gaOr",
  definition = function(.Object) {
    .Object <- lapply(
      X = .Object,
      FUN = GaNot
    )
    .Object <- as(.Object, "gaAnd")
  }
)
