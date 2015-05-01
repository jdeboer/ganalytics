#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
NULL

# ---- Not ----

setMethod(
  f = "Not",
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
    Not(x)
  }
)

setMethod(
  f = "Not",
  signature = ".expr",
  definition = function(.Object) {
    Operator(.Object) <- Not(Operator(.Object))
    return(.Object)
  }
)

setMethod(
  f = "!",
  signature = ".expr",
  definition = function(x) {
    Not(x)
  }
)

setMethod(
  f = "Not",
  signature = "orExpr",
  definition = function(.Object) {
    .Object <- lapply(
      X = .Object,
      FUN = Not
    )
    .Object <- do.call(And, .Object)
  }
)

setMethod(
  f = "!",
  signature = "orExpr",
  definition = function(x) {
    Not(x)
  }
)

setMethod(
  f = "Not",
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
    Not(x)
  }
)

# Backwards compatibility
#'@export GaNot
GaNot <- Not