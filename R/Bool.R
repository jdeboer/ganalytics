#' @include all-generics.R
#' @include all-coercions.R
#' @include comparator-classes.R
#' @include expression-classes.R
#' @include segment-classes.R
#' @include utils.R
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
NULL

# ---- Not ----

#' @describeIn Not
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

#' @describeIn Not
#' @param x the object to return the logical inverse of.
#' @export
setMethod("!", ".comparator", function(x) {Not(x)})

#' @describeIn Not
setMethod(
  f = "Not",
  signature = ".expr",
  definition = function(object) {
    comparator <- as(object, ".comparator")
    as(object, ".comparator") <- Not(comparator)
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
  signature = ".gaSegmentFilter",
  definition = function(object) {
    object@negation <- !object@negation
    object
  }
)

#' @describeIn Not
#' @export
setMethod("!", ".gaSegmentFilter", function(x) {Not(x)})

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
