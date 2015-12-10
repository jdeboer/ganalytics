#' @include Bool-generics.R
#' @include comparator-classes.R
#' @include expr-classes.R
#' @include segment-classes.R
#' @include utils.R
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
NULL

# ---- Not ----

#' @describeIn Not Return the inverse of the supplied comparison operator.
setMethod(
  f = "Not",
  signature = ".comparator",
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

#' @describeIn Not Return the inverse of the supplied comparator.
#' @param x the object to return the logical inverse of.
#' @export
setMethod("!", ".comparator", function(x) {Not(x)})

#' @describeIn Not Invert the comparator of a condition expression.
setMethod(
  f = "Not",
  signature = ".expr",
  definition = function(object) {
    comparator <- as(object, ".comparator")
    switch(
      comparator,
      "<>" = {
        assert_that(is(object, ".metExpr"))
        var <- as(object, ".var")
        operand <- sort(as(object, ".operand"))
        object <- Expr(var, "<", operand[1]) | Expr(var, ">", operand[2])
      },
      "[]" = {
        operand <- as(object, ".operand")
        operand <- paste0("^(", paste(operand, collapse = "|"), ")$")
        comparator <- "!~"
        var <- as(object, ".var")
        object <- Expr(var, comparator, operand)
      },
      as(object, ".comparator") <- Not(comparator)
    )
    object
  }
)

#' @describeIn Not Invert the comparator of the condition expression.
#' @export
setMethod("!", ".expr", function(x) {Not(x)})

#' @describeIn Not Invert an OR expression using De Morgan's Theorem.
setMethod(
  f = "Not",
  signature = "orExpr",
  definition = function(object) {
    do.call(And, lapply(object, Not))
  }
)

#' @describeIn Not Invert an OR expression using De Morgan's Theorem.
#' @export
setMethod("!", "orExpr", function(x) {Not(x)})

#' @describeIn Not Invert the negation of a segment filter condition, i.e.
#'   include <-> exclude
setMethod(
  f = "Not",
  signature = ".gaSegmentFilter",
  definition = function(object) {
    object@negation <- !object@negation
    object
  }
)

#' @describeIn Not Invert the negation of a segment filter condition, i.e.
#'   include <-> exclude
#' @export
setMethod("!", ".gaSegmentFilter", function(x) {Not(x)})

#' @describeIn Or OR two or more expressions.
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

#' @describeIn Or OR two expressions.
#' @param e1 first expression
#' @param e2 second expression
#' @export
setMethod("|", c(".compoundExpr", ".compoundExpr"), function(e1, e2) {Or(e1, e2)})

#' @describeIn And AND two or more expressions.
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

#' @describeIn And AND two expresisons.
#' @param e1 first expression
#' @param e2 second expression
#' @export
setMethod("&", c(".compoundExpr", ".compoundExpr"), function(e1, e2) {And(e1, e2)})

#' @describeIn xor Exclusive-OR two expressions.
#' @export
setMethod("xor", c(".compoundExpr", ".compoundExpr"), function(x, y) {(x | y) & (!x | !y)})
