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

#' @export
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

#' @export
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

#' @export
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

#' @export
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

# Create an Or from one or more metric or dimension expressions
# Takes one or more Metric or Dimension expressions
# as separate arguments or as a list.
# Returns an object of gaOr.

setMethod(
  f = "Or",
  signature = ".compoundExpr",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    exprList <- lapply(
      X = exprList,
      FUN = function(expr) {
        assert_that(!is(expr, "andExpr") | length(exprList) == 1)
        expr <- as(expr, "orExpr")
      }
    )
    exprList <- unlist(exprList, recursive = FALSE)
    new("orExpr", exprList)    
  }
)

#' @export
setMethod(
  f = "|",
  signature = c(".compoundExpr", ".compoundExpr"),
  definition = function(e1, e2) {
    Or(e1, e2)
  }
)

# Backwards compatibility
#' @export GaOr
GaOr <- Or

setMethod(
  f = "And",
  signature = ".compoundExpr",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    exprList <- lapply(
      X = exprList,
      FUN = function(expr) {
        if (is(expr, "andExpr")) {
          expr <- unlist(expr, recursive = FALSE)
          expr <- lapply(
            X = expr,
            FUN = function(exprB) {
              as(exprB, "orExpr")
            }
          )
        } else {
          as(object = expr, Class = "orExpr")
        }
      }
    )
    x <- sapply(
      X = exprList,
      FUN = function(expr) {
        !is(expr, "orExpr")
      }
    )
    exprList <- c(
      exprList[!x],
      unlist(
        exprList[x],
        recursive = FALSE
      )
    )
    new("andExpr", exprList)
  }
)

#' @export
setMethod(
  f = "&",
  signature = c(".compoundExpr", ".compoundExpr"),
  definition = function(e1, e2) {
    And(e1, e2)
  }
)

# Backwards compatibility
#' @export GaAnd
GaAnd <- And

