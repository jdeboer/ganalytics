#' @include segment-classes.R
#' @include init-methods.R
#' @include Sequence-generics.R
#' @include Segment-generics.R
#' @include segment-coerce.R
#' @include management-api-classes.R
#' @importFrom methods new setMethod
NULL

# ---- Include, Exclude ----

segment_scope_negate <- function(object, ..., scope, negate = NULL) {
  exprList <- list(object, ...)
  exprList <- lapply(exprList, function(expr){
    expr <- as(expr, ".gaSegmentFilter")
    if (!is.null(negate)) IsNegated(expr) <- negate
    expr
  })
  new("gaSegmentFilterList", exprList, scope = scope)
}

#' @describeIn Include Define an include segment filter using the supplied
#'   expression.
setMethod(
  f = "Include",
  signature = "ANY",
  definition = function(object, ..., scope) {
    segment_scope_negate(object, ..., scope = scope, negate = FALSE)
  }
)

#' @describeIn Exclude Define an exclude segment filter using the supplied
#'   expressions.
setMethod(
  f = "Exclude",
  signature = "ANY",
  definition = function(object, ..., scope) {
    segment_scope_negate(object, ..., scope = scope, negate = TRUE)
  }
)

# ---- SegmentConditionFilter, SegmentFilters, IsNegated ----

#' @describeIn SegmentConditionFilter Create a non-sequential segment condition
#'   filter from one or more expressions. All conditions within the filter must
#'   hold true within a single session.
setMethod(
  f = "SegmentConditionFilter",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- do.call("And", lapply(exprList, function(expr){as(expr, ".compoundExpr")}))
    new("gaSegmentConditionFilter", exprList, negation = negation)
  }
)

#' @describeIn SegmentConditionFilter Returns itself.
setMethod(
  f = "SegmentConditionFilter",
  signature = "gaSegmentConditionFilter",
  definition = function(object) {
    object
  }
)

#' @describeIn IsNegated Test whether a segment filter is negated, i.e. used to
#'   define an exclude filter for the segment.
setMethod(
  f = "IsNegated",
  signature = ".gaSegmentFilter",
  definition = function(object) {
    object@negation
  }
)

#' @describeIn IsNegated Set whether a segment filter should be negated, i.e.
#'   used as an exclude filter in a segment definition.
setMethod(
  f = "IsNegated<-",
  signature = c(".gaSegmentFilter", "logical"),
  definition = function(object, value) {
    object@negation <- value
    object
  }
)

#' @describeIn SegmentFilters Define a list of filters from one or more
#'   expressions applied using the specified scope.
setMethod(
  f = "SegmentFilters",
  signature = "ANY",
  definition = function(object, ..., scope) {
    segment_scope_negate(object, ..., scope = scope)
  }
)

#' @describeIn SegmentFilters Returns itself.
setMethod(
  f = "SegmentFilters",
  signature = "gaSegmentFilterList",
  definition = function(object) {
    object
  }
)

# ---- ScopeLevel, ScopeLevel<- ----

#' @describeIn ScopeLevel Return the scope of the supplied gaSegmentFilterList.
setMethod(
  f = "ScopeLevel",
  signature = "gaSegmentFilterList",
  definition = function(object) {
    object@scope
  }
)

#' @describeIn ScopeLevel Set the scope level of a gaSegmentFilterList to either
#'   "user" or "session" level.
setMethod(
  f = "ScopeLevel<-",
  signature = c("gaSegmentFilterList", "character"),
  definition = function(object, value) {
    object@scope <- value
    object
  }
)

# ---- PerSession, PerUser ----

#' @describeIn PerSession Set the scope of the supplied segment filters to
#'   session level.
setMethod(
  f = "PerSession",
  signature = "gaSegmentFilterList",
  definition = function(object, ...) {
    ScopeLevel(object) <- "sessions"
    SegmentFilters(object, ...)
  }
)

#' @describeIn PerSession Create a session level segment filter list from the
#'   supplied expressions, interpreted as condition filters.
setMethod(
  f = "PerSession",
  signature = "ANY",
  definition = function(object, ...) {
    SegmentFilters(object, ..., scope = "sessions")
  }
)

#' @describeIn PerSession Set the scope of the supplied metric condition to
#'   session-level.
setMethod(
  f = "PerSession",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perSession"
      object
    } else {
      SegmentFilters(object, ..., scope = "sessions")
    }
  }
)

#' @describeIn PerUser Set the scope of the supplied segment filter list to user
#'   level.
setMethod(
  f = "PerUser",
  signature = "gaSegmentFilterList",
  definition = function(object, ...) {
    ScopeLevel(object) <- "users"
    SegmentFilters(object, ...)
  }
)

#' @describeIn PerUser Create a user-level segment filter list from the supplied
#'   expressions, each interpreted as an include segment filter.
setMethod(
  f = "PerUser",
  signature = "ANY",
  definition = function(object, ...) {
    SegmentFilters(object, ..., scope = "users")
  }
)

#' @describeIn PerUser Set the scope of the supplied metric condition to
#'   user-level.
setMethod(
  f = "PerUser",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perUser"
      object
    } else {
      SegmentFilters(object, ..., scope = "users")
    }
  }
)

#' @describeIn PerHit Set the scope of the supplied metric condition to
#'   hit-level.
setMethod(
  f = "PerHit",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perHit"
      object
    } else {
      Sequence(And(object, ...))
    }
  }
)

#' @describeIn PerHit Create a single step sequence filter from the supplied
#'   expression.
setMethod(
  f = "PerHit",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    Sequence(And(object, ...))
  }
)

# ---- Segment, Segment<- ----

#' @describeIn Segment Interpret the supplied character or numeric value as a segment ID.
setMethod(
  f = "Segment",
  signature = "ANY",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segment returns itself.
setMethod(
  f = "Segment",
  signature = "gaDynSegment",
  definition = function(object) {
    object
  }
)

#' @describeIn Segment Create a non-sequential segment using the supplied
#'   expressions.
setMethod(
  f = "Segment",
  signature = ".compoundExpr",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr) {
      SegmentFilters(
        SegmentConditionFilter(expr),
        scope = scope
      )
    })
    new("gaDynSegment", exprList)
  }
)

#' @describeIn Segment Create a dynamic segment from the supplied segment
#'   filters.
setMethod(
  f = "Segment",
  signature = ".gaSegmentFilter",
  definition = function(object, ..., scope) {
    Segment(SegmentFilters(object, ..., scope = scope))
  }
)

#' @describeIn Segment Create a dynamic segment using the supplied scoped lists
#'   of segment filters.
setMethod(
  f = "Segment",
  signature = "gaSegmentFilterList",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, SegmentFilters, scope = scope)
    new("gaDynSegment", exprList)
  }
)

#' @describeIn Segment returns NULL
setMethod(
  f = "Segment",
  signature = "NULL",
  definition = function(object) {
    new("gaDynSegment", list())
  }
)

#' @describeIn Segment Change the definition of a dynamic segment using the
#'   supplied expression.
setMethod(
  f = "Segment<-",
  signature = c("gaDynSegment", "andExpr"),
  definition = function(object, value) {
    as(object, "andExpr") <- value
    object
  }
)

#' @describeIn Segment Change the definition of a dynamic segment.
setMethod(
  f = "Segment<-",
  signature = c("gaDynSegment", "ANY"),
  definition = function(object, value) {
    as(value, "gaDynSegment")
  }
)

#' @describeIn Segment Change the ID of the supplied segment.
setMethod(
  f = "Segment<-",
  signature = "gaSegmentId",
  definition = function(object, value) {
    to <- class(value)
    as(object, to) <- value
    object
  }
)


#' @describeIn Segment Returns itself
setMethod(
  f = "Segment",
  signature = "gaSegmentList",
  definition = function(object) {
    object
  }
)

#' @describeIn Segment Return the definition of the segment applied to the view.
setMethod(
  f = "Segment",
  signature = "gaQuery",
  definition = function(object) {
    object@segments
  }
)

#' @describeIn Segment Set the segments to be used witin a query.
setMethod(
  f = "Segment<-",
  signature = c("gaQuery", "ANY"),
  definition = function(object, value) {
    object@segments <- as(Segment(value), "gaSegmentList")
    object
  }
)

#' @describeIn Segment Return the segment ID of the supplied GA Management API
#'   user segment.
setMethod(
  f = "Segment",
  signature = "gaUserSegment",
  definition = function(object) {
    Segment(object$segmentId)
  }
)

