#' @include segment-classes.R
#' @include init-methods.R
#' @include Sequence-generics.R
#' @include Segment-generics.R
#' @include segment-coerce.R
#' @include management-api-classes.R
#' @importFrom methods new setMethod
NULL

# ---- Later, Then, First, Sequence ----

#' @describeIn Sequence
setMethod(
  f = "Later",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSegmentSequenceStep", as(object, "andExpr"), immediatelyPrecedes = FALSE)
  }
)

#' @describeIn Sequence
setMethod(
  f = "Then",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSegmentSequenceStep", as(object, "andExpr"), immediatelyPrecedes = TRUE)
  }
)

#' @describeIn Sequence
setMethod(
  f = "First",
  signature = ".compoundExpr",
  definition = function(object) {
    Then(object)
  }
)

#' @describeIn Sequence
setMethod(
  f = "Sequence",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSegmentSequenceStep")})
    new("gaSegmentSequenceFilter", exprList, negation = negation)
  }
)
