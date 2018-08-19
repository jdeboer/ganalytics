#' @include segment-classes.R
#' @include Sequence-generics.R
#' @include Segment-generics.R
#' @include segment-coerce.R
#' @include management-api-classes.R
#' @importFrom methods new setMethod
NULL

# ---- Later, Then, First, Sequence ----

#' @describeIn Sequence Defines a sequence step using the supplied expression that does
#'   not need to be immediately at the start nor immediately following any
#'   preceding step.
setMethod(
  f = "Later",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSegmentSequenceStep", as(object, "andExpr"), immediatelyPrecedes = FALSE)
  }
)

#' @describeIn Sequence Defines a sequential step using the supplied expression
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

#' @describeIn Sequence Return a sequence of one or more steps using the
#'   supplied expression(s) define the steps, where those step can occur
#'   anywhere within the sequences of interactions being matched, but in the
#'   order specified.
setMethod(
  f = "Sequence",
  signature = "ANY",
  definition = function(object, ..., negation, scope) {
    exprList <- unnest_objects(object, ..., class = "gaSegmentSequenceFilter")
    exprList <- lapply(exprList, as, "gaSegmentSequenceStep")
    exprList <- new("gaSegmentSequenceFilter", exprList)
    if(missing(negation) & missing(scope)) {
      setSegmentFilterScopeNegation(exprList)
    } else if (!missing(negation) & missing(scope)) {
      setSegmentFilterScopeNegation(exprList, negation = negation)
    } else if (missing(negation) & !missing(scope)) {
      setSegmentFilterScopeNegation(exprList, scope = scope)
    } else if (!missing(negation) & !missing(scope)) {
      setSegmentFilterScopeNegation(exprList, negation = negation, scope = scope)
    }
  }
)
