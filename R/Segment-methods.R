#' @include segment-classes.R
#' @include init-methods.R
#' @include Sequence-generics.R
#' @include Segment-generics.R
#' @include segment-coerce.R
#' @include management-api-classes.R
#' @importFrom methods new setMethod
NULL

# ---- Include, Exclude ----

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

# ---- SegmentConditionFilter, DynSegment, IsNegated ----

#' @describeIn SegmentConditionFilter Create a non-sequential segment condition
#'   filter from one or more expressions. All conditions within the filter must
#'   hold true within a single session if applied to a gaDynSegment
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

#' @describeIn DynSegment Defines a list of filters from one or more
#'   expressions applied using the specified scope.
setMethod(
  f = "DynSegment",
  signature = "ANY",
  definition = function(object, ...) {
    exprList <- list(object, ...)
    nested <- sapply(exprList, is, "gaDynSegment")
    segment_filter_list <- lapply(exprList[!nested], function(expr){
      as(expr, ".gaSegmentFilter")
    })
    nested_segment_filters <- unlist(exprList[nested], recursive = FALSE)
    segment_filter_list <- c(
      segment_filter_list,
      nested_segment_filters
    )
    new("gaDynSegment", segment_filter_list)
  }
)

#' @describeIn DynSegment Returns itself.
setMethod(
  f = "DynSegment",
  signature = c("gaDynSegment"),
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

#' @describeIn ScopeLevel Set the scope level of a gaDynSegment to either
#'   "user" or "session" level.
setMethod(
  f = "ScopeLevel<-",
  signature = c("gaDynSegment", "character"),
  definition = function(object, value) {
    object <- lapply(object, function(segmentFilter) {
      ScopeLevel(segmentFilter) <- value
      segmentFilter
    })
    object <- new("gaDynSegment", object)
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
    SegmentConditionFilter(object, ..., scope = "sessions")
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
      SegmentConditionFilter(object, ..., scope = "sessions")
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
    SegmentConditionFilter(object, ..., scope = "users")
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
      SegmentConditionFilter(object, ..., scope = "users")
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

#' @describeIn Segment Interpret the supplied numeric value as a segment ID.
setMethod(
  f = "Segment",
  signature = "numeric",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segment Interpret the supplied character value as a segment ID.
setMethod(
  f = "Segment",
  signature = "character",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segment Create a non-sequential segment using the supplied
#'   expressions.
setMethod(
  f = "Segment",
  signature = "ANY",
  definition = function(object, ...) {
    dyn_segment_def <- list(object, ...)
    segment_filter_list <- lapply(dyn_segment_def, function(segment_filter_list) {
      as(segment_filter_list, ".gaSegmentFilter")
    })
    new(
      "gaDynSegment",
      segment_filter_list
    )
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

#' @describeIn Segment Return the segment ID of the supplied GA Management API
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
      "gaDynSegment",
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

