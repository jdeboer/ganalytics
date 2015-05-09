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
  definition = function(object) {
    as(object, "character") <- switch(
      object,
      "==" = "!=",
      "!=" = "==",
      "<"  = ">=",
      ">=" = "<",
      ">" = "<=",
      "<=" = ">",
      "!~" = "=~",
      "=~" = "!~",
      "=@" = "!@",
      "!@" = "=@",
      stop(paste(object, "cannot be NOTed."))
    )
    object
  }
)

#' @export
setMethod("!", ".operator", function(x) {Not(x)})

setMethod(
  f = "Not",
  signature = ".expr",
  definition = function(object) {
    operator <- as(object, ".operator")
    as(object, ".operator") <- Not(operator)
    object
  }
)

#' @export
setMethod("!", ".expr", function(x) {Not(x)})

setMethod(
  f = "Not",
  signature = "orExpr",
  definition = function(object) {
    do.call(And, lapply(object, Not))
  }
)

#' @export
setMethod("!", "orExpr", function(x) {Not(x)})

setMethod(
  f = "Not",
  signature = ".gaSimpleOrSequence",
  definition = function(object) {
    object@negation <- !object@negation
    object
  }
)

#' @export
setMethod("!", ".gaSimpleOrSequence", function(x) {Not(x)})

# Create an Or from one or more metric or dimension expressions
# Takes one or more Metric or Dimension expressions
# as separate arguments or as a list.
# Returns an object of gaOr.

setMethod(
  f = "Or",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    exprList <- list(object, ...)
    exprList <- lapply(
      X = exprList,
      FUN = function(expr) {
        tryCatch(
          assert_that(!is(expr, "andExpr") | length(exprList) == 1),
          error = function(e) {
            stop("ANDed expressions cannot be ORed.\n", e)
          }
        )
        expr <- as(expr, "orExpr")
      }
    )
    exprList <- unlist(exprList, recursive = FALSE)
    new("orExpr", exprList)
  }
)

#' @export
setMethod("|", c(".compoundExpr", ".compoundExpr"), function(e1, e2) {Or(e1, e2)})

setMethod(
  f = "And",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    exprList <- list(object, ...)
    exprList <- lapply(
      X = exprList,
      FUN = function(expr) {
        if (is(expr, "andExpr")) {
          expr <- unlist(expr, recursive = FALSE)
          expr <- lapply(expr, as, "orExpr")
        } else {
          as(expr, "orExpr")
        }
      }
    )
    nested <- !sapply(exprList, is, "orExpr")
    exprList <- c(
      exprList[!nested],
      unlist(
        exprList[nested],
        recursive = FALSE
      )
    )
    new("andExpr", exprList)
  }
)

#' @export
setMethod("&", c(".compoundExpr", ".compoundExpr"), function(e1, e2) {And(e1, e2)})

#' @export
setMethod("xor", c(".compoundExpr", ".compoundExpr"), function(x, y) {(x | y) & (!x | !y)})

# Backwards compatibility
#'@export GaNot
GaNot <- Not
#' @export GaOr
GaOr <- Or
#' @export GaAnd
GaAnd <- And
