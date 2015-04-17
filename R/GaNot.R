#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
NULL

# ---- GaNot ----

setMethod(
  f = "GaNot",
  signature = ".operator",
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
    } else stop(paste(.Object, "cannot be NOTed."))
  }
)

setMethod(
  f = "GaNot",
  signature = ".expr",
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
    .Object <- do.call(GaAnd, .Object)
  }
)

setMethod(
  f = "GaNot",
  signature = ".gaSimpleOrSequence",
  definition = function(.Object) {
    .Object@negation <- !.Object@negation
    return(.Object)
  }
)
