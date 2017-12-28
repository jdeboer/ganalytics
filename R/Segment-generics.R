#' SegmentConditionFilter.
#'
#' Create a new gaSegmentConditionFilter object
#'
#' @param object An expression to be used as a non-sequential segment condition.
#' @param ... Other expressions to be ANDed to the first expression provided.
#' @param negation Deprecated. Logical TRUE or FALSE to match segments where this conditon
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
#' Define a segment filter with its negation flag is set to FALSE.
#'
#' @param object a segment filter condition or sequence to include.
#' @return a .gaSegmentFilter object with its negate slot set to FALSE.
#'
#' @export
setGeneric(
  "Include",
  function(object) {},
  valueClass = ".gaSegmentFilter",
  useAsDefault = FALSE
)

#' Exclude.
#'
#' Define a segment filter with its negation flag is set to TRUE.
#'
#' @param object a segment filter condition or sequence to exclude.
#' @return a .gaSegmentFilter object with its negate slot set to TRUE.
#'
#' @export
setGeneric(
  "Exclude",
  function(object) {},
  valueClass = ".gaSegmentFilter",
  useAsDefault = TRUE
)

#' IsNegated.
#'
#' Tests whether a segment filter is negated.
#'
#' @param object an object belonging to the superclass \code{.gaSegmentFilter}.
#'
#' @rdname IsNegated
#' @export
setGeneric(
  "IsNegated",
  function(object) {},
  valueClass = "logical",
  useAsDefault = FALSE
)

#' IsNegated<-.
#'
#' Sets the negation flag of a segment filter.
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

#' SegmentFilters.
#'
#' Combine one or more segment condition filters and/or sequence filters into
#' a gaSegmentFilterList that is scoped to either 'user' or 'session' level.
#'
#' A segment filter is either sequential or non-sequential conditions. Sequential
#' and non-sequential conditoins can be combined using this function.
#'
#' @param object The first filter to include in the segment definition.
#' @param ... Additional filters to be included in the segment definition.
#' @param scope The scope of the resulting gaSegmentFilterList, either 'user' or
#' 'session' level.
#' @return a gaSegmentFilterList object.
#'
#' @export
setGeneric(
  "SegmentFilters",
  function(object, ..., scope = "sessions") {},
  valueClass = "gaSegmentFilterList",
  useAsDefault = FALSE
)

#' PerProduct.
#'
#' Set the scope of a gaMetExpr object to product-level.
#'
#' @param object a gaMetExpr object to coerce to hit-level
#' @return a gaMetExpr object.
#'
#' @export
setGeneric(
  "PerProduct",
  function(object){},
  valueClass = "gaSegMetExpr",
  useAsDefault = FALSE
)

#' PerHit.
#'
#' Set the scope of a gaMetExpr object to hit-level, or transforms a condition
#' filter to a sequence filter of length one (i.e. a combination of conditions for
#' matching a single hit).
#'
#' @param object a gaMetExpr object to coerce to hit-level or if multiple expressions
#' are provided, then the first expression to combine into a single step of sequence
#' filter.
#' @param ... Further expressions to be included in the filter definition if defining a
#' sequence filter of length one.
#' @return a gaMetExpr or gaSegmentSequenceFilter.
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
#' Alternatively, an dimension expression or segment filter to coerce into a session
#' scoped gaSegmentFilterList.
#' @param ... Other filters to include in the gaSegmentFilterList.
#' @return a gaMetExpr or gaSegmentFilterList.
#'
#' To define a gaSegmentFilterList using metric expressions rather than setting the
#' scope of the metric expression itself, wrap the metric expression in an
#' \code{Include} or \code{Exclude} call.
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
#' Alternatively, an dimension expression or segment filter to coerce into a user
#' scoped gaSegmentFilterList.
#' @param ... Other filters to include in the gaSegmentFilterList.
#' @return a gaMetExpr or gaSegmentFilterList.
#'
#' To define a gaSegmentFilterList using metric expressions rather than setting the
#' scope of the metric expression itself, wrap the metric expression in an
#' \code{Include} or \code{Exclude} call.
#'
#' @export
setGeneric(
  "PerUser",
  function(object, ...){},
  valueClass = c("gaSegmentFilterList", "gaSegMetExpr"),
  useAsDefault = FALSE
)

#' Segment.
#'
#' Define a segment for use in a query's segment list.
#'
#' @param object a segment or other object that can be coerced into
#' a segment, including dynamic segments, built-in and/or custom
#' segments by their ID.
#' @param ... other segment conditions, filters or filter lists to include
#' in the segment's definition (ANDed)
#' @return an object belonging to the .gaSegment superclass.
#'
#' @name Segment
#' @export
setGeneric(
  "Segment",
  function(object, ...) {},
  valueClass = ".gaSegment",
  useAsDefault = FALSE
)

#' Segments.
#'
#' Get the list of segments from the object or coerce the supplied objects into a
#' a named list of segments.
#'
#' @param object A query object to extract the segment list from.
#' @param ... Alternatively, provide one or more named arguments
#' (segments or objects that can be coerced into segments)
#' including dynamic segments, built-in and/or custom segments by
#' their ID.
#' @return a gaSegmentList
#'
#' @export
#' @rdname Segments
setGeneric(
  "Segments",
  function(object, ...) {},
  valueClass = c("gaSegmentList"),
  useAsDefault = FALSE
)

#' Segments<-.
#'
#' Set the segments of the query object.
#'
#' @param value The segment definition or ID or a list of segments.
#'
#' @export
#' @rdname Segments
setGeneric(
  "Segments<-",
  function(object, value) {
    object <- standardGeneric("Segments<-")
    validObject(object)
    object
  }
)
