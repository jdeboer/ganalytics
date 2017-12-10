#' SegmentConditionFilter.
#'
#' Create a new gaSegmentConditionFilter object
#'
#' @param object An expression to be used as a non-sequential segment condition.
#' @param ... Other expressions to be ANDed to the first expression provided.
#' @param negation Logical TRUE or FALSE to match segments where this conditon
#'   has not been met.
#' @return a gaSegmentConditionFilter object.
#'
#' @export
setGeneric(
  "SegmentConditionFilter",
  function(object, ..., negation = FALSE) {},
  valueClass = "gaSegmentConditionFilter",
  useAsDefault = FALSE
)

#' Include.
#'
#' One or more segment conditions or sequences to include from the defined user
#' or session segment.
#'
#' @param object a condition or sequence to include
#' @param ... further conditions or sequences to include, i.e. ANDed.
#' @param scope the scope of the returned gaSegmentList, either "users" OR
#'   "sessions".
#' @return a .gaSegmentFilter object with its negate slot set to FALSE.
#'
#' @export
setGeneric(
  "Include",
  function(object, ...) {}, # , scope = "sessions"
  valueClass = ".gaSegmentFilter", #
  useAsDefault = FALSE
)

#' Exclude.
#'
#' One or more segment conditions or sequences to exclude from the defined user
#' or session segment.
#'
#' @param object a condition or sequence to exclude
#' @param ... further conditions or sequences to exclude.
#' @param scope the scope of the returned gaSegmentList, either "users" OR
#'   "sessions".
#' @return a .gaSegmentFilter object with its negate slot set to TRUE.
#'
#' @export
setGeneric(
  "Exclude",
  function(object, ...) {}, # , scope = "sessions"
  valueClass = ".gaSegmentFilter", # gaSegmentFilterList
  useAsDefault = TRUE
)

#' SegmentFilters.
#'
#' Create a new gaSegmentFilterList object
#'
#' A segment condition is either sequential or non-sequential. Sequential and
#' non-sequential conditoins can be combined using this function.
#'
#' @param object The first condition to be included in the segments definition.
#' @param ... Other conditions to be included in the segment definition.
#' @param scope The scope of this condition, either 'user' or 'session' level.
#' @return a gaSegmentFilterList object.
#'
#' @export
setGeneric(
  "SegmentFilters",
  function(object, ..., scope = "sessions") {},
  valueClass = "gaSegmentFilterList",
  useAsDefault = FALSE
)

#' IsNegated.
#'
#' Tests whether a segment filter is negated.
#'
#' @param object an object belonging to the superclass \code{.gaSegmentFilter}.
#'
#' @export
setGeneric(
  "IsNegated",
  function(object) {},
  valueClass = "logical",
  useAsDefault = FALSE
)

#' IsNegated<-.
#'
#' Sets whether a segment filter should be negated.
#'
#' @param value the value of the negation slot, either \code{TRUE} or
#'   \code{FALSE}.
#'
#' @rdname IsNegated
#' @export
setGeneric(
  "IsNegated<-",
  function(object, value) {
    object <- standardGeneric("IsNegated<-")
    validObject(object)
    object
  }
)

#' PerHit.
#'
#' Set the scope of a gaMetExpr object to hit-level, or transforms a condition
#' filter to a sequence filter of length one (i.e. conditions to match a single
#' hit).
#'
#' @param object a gaMetExpr object to coerce to user-level.
#' @param ... Other conditions to be included in the segment definition.
#'
#' @export
setGeneric(
  "PerHit",
  function(object, ...){},
  valueClass = c("gaSegMetExpr", "gaSegmentSequenceFilter"),
  useAsDefault = FALSE
)

#' PerSession.
#'
#' Set the scope of a gaSegmentFilterList or gaMetExpr object to session-level.
#'
#' @param object a gaSegmentFilterList or gaMetExpr object to coerce to session-level.
#' @param ... Other conditions to be included in the segment definition.
#'
#' @export
setGeneric(
  "PerSession",
  function(object, ...){},
  valueClass = c("gaSegmentFilterList", "gaSegMetExpr"),
  useAsDefault = FALSE
)

#' PerUser.
#'
#' Set the scope of a gaSegmentFilterList or gaMetExpr object to user-level.
#'
#' @param object a gaSegmentFilterList or gaMetExpr object to coerce to user-level.
#' @param ... Other conditions to be included in the segment definition.
#'
#' @export
setGeneric(
  "PerUser",
  function(object, ...){},
  valueClass = c("gaSegmentFilterList", "gaSegMetExpr"),
  useAsDefault = FALSE
)

#' ScopeLevel.
#'
#' Get the scope level of a gaDynSegment or gaMetExpr
#'
#' @param object Segment condition or combined segment conditions or metric
#'   expression.
#' @param value If a new scope level is supplied, then this function will return
#'   an updated copy of the supplied object with the new scope applied.
#'
#' @export
#' @rdname ScopeLevel
setGeneric(
  "ScopeLevel",
  function(object, value) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' ScopeLevel<-.
#'
#' Set the scope level of a gaDynSegment or a gaMetExpr
#' For segments, one of 'users' or 'sessions'
#' For metric expressions one of 'perUser', 'perSession' or 'perHit'
#'
#' @export
#' @rdname ScopeLevel
setGeneric(
  "ScopeLevel<-",
  function(object, value) {
    object <- standardGeneric("ScopeLevel<-")
    validObject(object)
    object
  }
)

#' Segment.
#'
#' Get the segment.
#'
#' @param object An expression to coerce to a segment definition or segment ID
#' @param ... Other expressions to combine with the first expression, if
#'   appropriate.
#' @param scope The scope level to apply to the resulting segment definition.
#'
#' @export
#' @rdname Segment
setGeneric(
  "Segment",
  function(object, ..., scope = "sessions") {},
  valueClass = c(".gaSegment", "gaSegmentList"),
  useAsDefault = FALSE
)

#' Segment<-.
#'
#' Set the segment
#'
#' @param value The segment definition or ID to set the segment parameter to.
#'
#' @export
#' @rdname Segment
setGeneric(
  "Segment<-",
  function(object, value) {
    object <- standardGeneric("Segment<-")
    validObject(object)
    object
  }
)
