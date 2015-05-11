#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
NULL

# ---- Not ----

#' @describeIn Not
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

#' @describeIn Not
#' @param x the object to return the logical inverse of.
#' @export
setMethod("!", ".operator", function(x) {Not(x)})

#' @describeIn Not
setMethod(
  f = "Not",
  signature = ".expr",
  definition = function(object) {
    operator <- as(object, ".operator")
    as(object, ".operator") <- Not(operator)
    object
  }
)

#' @describeIn Not
#' @export
setMethod("!", ".expr", function(x) {Not(x)})

#' @describeIn Not
setMethod(
  f = "Not",
  signature = "orExpr",
  definition = function(object) {
    do.call(And, lapply(object, Not))
  }
)

#' @describeIn Not
#' @export
setMethod("!", "orExpr", function(x) {Not(x)})

#' @describeIn Not
setMethod(
  f = "Not",
  signature = ".gaSimpleOrSequence",
  definition = function(object) {
    object@negation <- !object@negation
    object
  }
)

#' @describeIn Not
#' @export
setMethod("!", ".gaSimpleOrSequence", function(x) {Not(x)})

# Create an Or from one or more metric or dimension expressions
# Takes one or more Metric or Dimension expressions
# as separate arguments or as a list.
# Returns an object of gaOr.

#' @describeIn Or
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

#' @describeIn Or
#' @param e1 first expression
#' @param e2 second expression
#' @export
setMethod("|", c(".compoundExpr", ".compoundExpr"), function(e1, e2) {Or(e1, e2)})

#' @describeIn And
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

#' @describeIn And
#' @param e1 first expression
#' @param e2 second expression
#' @export
setMethod("&", c(".compoundExpr", ".compoundExpr"), function(e1, e2) {And(e1, e2)})

#' @describeIn xor
#' @export
setMethod("xor", c(".compoundExpr", ".compoundExpr"), function(x, y) {(x | y) & (!x | !y)})

# Backwards compatibility
#' GaNot (Deprecated, use Not or ! instead).
#'
#' @export GaNot
#' @param ... arguments passed to \code{Not}
#' @rdname GaNot
GaNot <- function(...){Not(...)}

#' GaOr (Deprecated, use Or or | instead).
#' @export GaOr
#' @param ... arguments passed to \code{Or}
#' @rdname GaOr
GaOr <- function(...){Or(...)}

#' GaAnd (Deprecated, use And or & instead).
#' @export GaAnd
#' @param ... arguments passed to \code{And}
#' @rdname GaAnd
GaAnd <- function(...){And(...)}
