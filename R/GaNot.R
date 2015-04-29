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
      Operator(.Object) <- "!="
      return(.Object)
    } else if (.Object == "!=") {
      Operator(.Object) <- "=="
      return(.Object)
    } else if (.Object == "<") {
      Operator(.Object) <- ">="
      return(.Object)
    } else if (.Object == ">=") {
      Operator(.Object) <- "<"
      return(.Object)
    } else if (.Object == ">") {
      Operator(.Object) <- "<="
      return(.Object)
    } else if (.Object == "<=") {
      Operator(.Object) <- ">"
      return(.Object)
    } else if (.Object == "!~") {
      Operator(.Object) <- "=~"
      return(.Object)
    } else if (.Object == "=~") {
      Operator(.Object) <- "!~"
      return(.Object)
    } else if (.Object == "=@") {
      Operator(.Object) <- "!@"
      return(.Object)
    } else if (.Object == "!@") {
      Operator(.Object) <- "=@"
      return(.Object)
    } else stop(paste(.Object, "cannot be NOTed."))
  }
)

setMethod(
  f = "!",
  signature = ".operator",
  definition = function(x) {
    GaNot(x)
  }
)

setMethod(
  f = "GaNot",
  signature = ".expr",
  definition = function(.Object) {
    Operator(.Object) <- GaNot(Operator(.Object))
    return(.Object)
  }
)

setMethod(
  f = "!",
  signature = ".expr",
  definition = function(x) {
    GaNot(x)
  }
)

setMethod(
  f = "GaNot",
  signature = "orExpr",
  definition = function(.Object) {
    .Object <- lapply(
      X = .Object,
      FUN = GaNot
    )
    .Object <- do.call(GaAnd, .Object)
  }
)

setMethod(
  f = "!",
  signature = "orExpr",
  definition = function(x) {
    GaNot(x)
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

setMethod(
  f = "!",
  signature = ".gaSimpleOrSequence",
  definition = function(x) {
    GaNot(x)
  }
)
