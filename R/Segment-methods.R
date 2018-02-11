#' @include segment-classes.R
#' @include init-methods.R
#' @include Sequence-generics.R
#' @include Segment-generics.R
#' @include segment-coerce.R
#' @include management-api-classes.R
#' @importFrom methods new setMethod
NULL

# ---- Include, Exclude ----

# This function takes one or more gaSegmentFilter, expressions or conditions
# and combines them into a gaSegmentFilterList, which has an overarching scope applied.
segment_scope_negate <- function(object, ..., scope, negate = NULL) {
  exprList <- list(object, ...)
  nested <- sapply(exprList, is, "gaSegmentFilterList")
  segment_filter_list <- lapply(exprList[!nested], function(expr){
    expr <- as(expr, ".gaSegmentFilter")
    if (!is.null(negate)) IsNegated(expr) <- negate
    expr
  })
  nested_segment_filters <- unlist(exprList[nested], recursive = FALSE)
  if (!is.null(negate)) {
    nested_segment_filters <- lapply(nested_segment_filters, function(segment_filter) {
      IsNegated(segment_filter) <- negate
      segment_filter
    })
  }
  segment_filter_list <- c(
    segment_filter_list,
    nested_segment_filters
  )
  new("gaSegmentFilterList", segment_filter_list, scope = scope)
}

#' @describeIn Include Define an include segment filter using the supplied
#'   expression.
setMethod(
  f = "Include",
  signature = "ANY",
  definition = function(object, scope) {
    object <- as(object, ".gaSegmentFilter")
    if(!missing(scope)) object@scope <- scope
    object@negation <- FALSE
    validObject(object)
    object
  }
)

#' @describeIn Exclude Define an exclude segment filter using the supplied
#'   expressions.
setMethod(
  f = "Exclude",
  signature = "ANY",
  definition = function(object, scope) {
    object <- as(object, ".gaSegmentFilter")
    if(!missing(scope)) object@scope <- scope
    object@negation <- TRUE
    validObject(object)
    object
  }
)

# ---- SegmentConditionFilter, SegmentFilters, IsNegated ----

#' @describeIn SegmentConditionFilter Create a non-sequential segment condition
#'   filter from one or more expressions. All conditions within the filter must
#'   hold true within a single session if applied to a gaSegmentFilterList
#'   scoped at session-level, or to a single hit if scoped at user-level.
setMethod(
  f = "SegmentConditionFilter",
  signature = "ANY",
  definition = function(object, ..., negation, scope) {
    exprList <- list(object, ...)
    nested <- sapply(exprList, is, "gaSegmentConditionFilter")
    exprList <- c(exprList[!nested], unlist(exprList[nested], recursive = FALSE))
    exprList <- do.call("And", lapply(exprList, function(expr){as(expr, ".compoundExpr")}))
    new("gaSegmentConditionFilter", exprList, negation = negation, scope = scope)
  }
)

#' @describeIn IsNegated Tests whether a segment filter is negated, i.e. used to
#'   define an exclude filter for the segment.
setMethod(
  f = "IsNegated",
  signature = ".gaSegmentFilter",
  definition = function(object) {
    object@negation
  }
)

#' @describeIn IsNegated Sets whether a segment filter should be negated, i.e.
#'   used as an exclude filter in a segment definition.
setMethod(
  f = "IsNegated<-",
  signature = c(".gaSegmentFilter", "logical"),
  definition = function(object, value) {
    object@negation <- value
    object
  }
)

#' @describeIn SegmentFilters Defines a list of filters from one or more
#'   expressions applied using the specified scope.
setMethod(
  f = "SegmentFilters",
  signature = "ANY",
  definition = function(object, ...) {
    segment_scope_negate(object, ...)
  }
)

#' @describeIn SegmentFilters Returns itself.
setMethod(
  f = "SegmentFilters",
  signature = c("gaSegmentFilterList", "missing"),
  definition = function(object) {
    object
  }
)

# ---- ScopeLevel, ScopeLevel<- ----

#' @describeIn ScopeLevel Returns the scope of the supplied .gaSegmentFilter.
setMethod(
  f = "ScopeLevel",
  signature = c(".gaSegmentFilter"),
  definition = function(object) {
    object@scope
  }
)

#' @describeIn ScopeLevel Returns the scope of the supplied gaSegmentFilterList.
setMethod(
  f = "ScopeLevel",
  signature = "gaSegmentFilterList",
  definition = function(object) {
    object@scope
  }
)

#' @describeIn ScopeLevel Set the scope level of a .gaSegmentFilter to either
#'   "user" or "session" level.
setMethod(
  f = "ScopeLevel<-",
  signature = c(".gaSegmentFilter", "character"),
  definition = function(object, value) {
    object@scope <- value
    validObject(object)
    object
  }
)

#' @describeIn ScopeLevel Set the scope level of a gaSegmentFilterList to either
#'   "user" or "session" level.
setMethod(
  f = "ScopeLevel<-",
  signature = c("gaSegmentFilterList", "character"),
  definition = function(object, value) {
    object@scope <- value
    validObject(object)
    object
  }
)

# ---- PerSession, PerUser ----

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
  signature = c("gaMetExpr"),
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perSession"
      object
    } else {
      SegmentFilters(object, ..., scope = "sessions")
    }
  }
)

#' @describeIn PerSession Set the scope of the supplied non-standard-evaluation
#'   metric condition to session-level.
setMethod(
  f = "PerSession",
  signature = c("formula"),
  definition = function(object, ...) {
    PerSession(Expr(object), ...)
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
  signature = c("gaMetExpr"),
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perUser"
      object
    } else {
      SegmentFilters(object, ..., scope = "users")
    }
  }
)

#' @describeIn PerUser Set the scope of the supplied non-standard-evaluation
#'   metric condition to user-level.
setMethod(
  f = "PerUser",
  signature = c("formula"),
  definition = function(object, ...) {
    PerUser(Expr(object), ...)
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

#' @describeIn PerHit Set the scope of the supplied metric condition to
#'   hit-level.
setMethod(
  f = "PerHit",
  signature = c("gaMetExpr"),
  definition = function(object, ...) {
    if (missing(...)) {
      ScopeLevel(object) <- "perHit"
      object
    } else {
      Sequence(And(object, ...))
    }
  }
)

#' @describeIn PerHit Set the scope of the supplied non-standard-evaluation
#'   metric condition to hit-level.
setMethod(
  f = "PerHit",
  signature = c("formula"),
  definition = function(object, ...) {
    PerHit(Expr(object), ...)
  }
)

#' @describeIn PerProduct Set the scope of the supplied metric condition to
#'   product-level.
setMethod(
  f = "PerProduct",
  signature = c("gaMetExpr"),
  definition = function(object) {
    ScopeLevel(object) <- "perProduct"
    object
  }
)

#' @describeIn PerProduct Set the scope of the supplied non-standard-evaluation
#'   metric condition to product-level.
setMethod(
  f = "PerProduct",
  signature = c("formula"),
  definition = function(object) {
    PerProduct(Expr(object))
  }
)

# ---- Segment ----

#' @describeIn Segments Interpret the supplied numeric value as a segment ID.
setMethod(
  f = "Segment",
  signature = "numeric",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segments Interpret the supplied character value as a segment ID.
setMethod(
  f = "Segment",
  signature = "character",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segments Create a non-sequential segment using the supplied
#'   expressions.
setMethod(
  f = "Segment",
  signature = "ANY",
  definition = function(object, ...) {
    dyn_segment_def <- list(object, ...)
    segment_filter_list <- lapply(dyn_segment_def, function(segment_filter_list) {
      as(segment_filter_list, "gaSegmentFilterList")
    })
    new(
      "gaDynSegment",
      segment_filter_list
    )
  }
)

#' @describeIn Segments returns NULL
setMethod(
  f = "Segment",
  signature = "NULL",
  definition = function(object) {
    new("gaDynSegment", list())
  }
)

#' @describeIn Segments Return the segment ID of the supplied GA Management API
#'   user segment.
setMethod(
  f = "Segment",
  signature = "gaUserSegment",
  definition = function(object) {
    Segment(object$segmentId)
  }
)

# ---- Segments, Segments<- ----

#' @describeIn Segments Returns itself
setMethod(
  f = "Segments",
  signature = "gaSegmentList",
  definition = function(object) {
    object
  }
)

#' @describeIn Segments Return the definition of the segment applied to the view.
setMethod(
  f = "Segments",
  signature = "gaQuery",
  definition = function(object) {
    object@segments
  }
)

#' @describeIn Segments Coerce an object into a segmentList of length 1.
setMethod(
  f = "Segments",
  signature = "ANY",
  definition = function(object) {
    if(inherits(object, c(
      "gaUserSegment",
      ".gaSegment",
      "character",
      "numeric",
      "gaSegmentFilterList",
      ".compoundExpr"
    ))) {
      new("gaSegmentList", list(segment = Segment(object)))
    } else {
      stopifnot(inherits(object, c("list", "NULL")))
      new("gaSegmentList", object)
    }
  }
)

#' @describeIn Segments Set the segments to be used witin a query.
setMethod(
  f = "Segments<-",
  signature = c("gaQuery", "ANY"),
  definition = function(object, value) {
    # Need to define coercions to .gaSegment from char and numeric
    object@segments <- Segments(value)
    object
  }
)

