#' @include segment-classes.R
#' @include init-methods.R
#' @include Sequence-generics.R
#' @include Segment-generics.R
#' @include segment-coerce.R
#' @include management-api-classes.R
#' @importFrom methods new setMethod
NULL

# ---- Later, Then, First, Sequence ----

#' @describeIn Sequence Defines a sequence step using the supplied expression that does
#'   not need to be immediately at the start nor immediately following any
#'   preceeding step.
setMethod(
  f = "Later",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSegmentSequenceStep", as(object, "andExpr"), immediatelyPrecedes = FALSE)
  }
)

#' @describeIn Sequence Defines a sequencial step using the supplied expression
#'   that should immediately follow any preceding step or be the very first
#'   required interaction in any sequences being matched to this sequential
#'   segment definition.
setMethod(
  f = "Then",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSegmentSequenceStep", as(object, "andExpr"), immediatelyPrecedes = TRUE)
  }
)

#' @describeIn Sequence Alias to Then.
setMethod(
  f = "First",
  signature = ".compoundExpr",
  definition = function(object) {
    Then(object)
  }
)

#' @describeIn Sequence Return a seuqnece of of just one step using the
#'   supplied expression to define that step, where that step can occur anywhere
#'   within the sequences of interactions being matched.
setMethod(
  f = "Sequence",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSegmentSequenceStep")})
    new("gaSegmentSequenceFilter", exprList, negation = negation)
  }
)
