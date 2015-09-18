#' @include segment-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
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

# ---- Include, Exclude ----

#' @describeIn Include
setMethod(
  f = "Include",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    exprList <- list(object, ...)
    do.call(
      SegmentFilters, lapply(exprList, SegmentConditionFilter, negation = FALSE)
    )
  }
)

#' @describeIn Exclude
setMethod(
  f = "Exclude",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    exprList <- list(object, ...)
    do.call(
      SegmentFilters, lapply(exprList, SegmentConditionFilter, negation = TRUE)
    )
  }
)

#' @describeIn Include
setMethod(
  f = "Include",
  signature = "gaSegmentSequenceFilter",
  definition = function(object, ...) {
    SegmentConditionFilter(object, ..., negation = FALSE)
  }
)

#' @describeIn Exclude
setMethod(
  f = "Exclude",
  signature = "gaSegmentSequenceFilter",
  definition = function(object, ...) {
    SegmentConditionFilter(object, ..., negation = TRUE)
  }
)

# ---- SegmentConditionFilter, SegmentFilters, IsNegated ----

#' @describeIn SegmentConditionFilter
setMethod(
  f = "SegmentConditionFilter",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- do.call("And", lapply(exprList, function(expr){as(expr, "andExpr")}))
    new("gaSegmentConditionFilter", exprList, negation = negation)
  }
)

#' @describeIn IsNegated
setMethod(
  f = "IsNegated",
  signature = ".gaSegmentFilter",
  definition = function(object) {
    object@negation
  }
)

#' @describeIn SegmentFilters
setMethod(
  f = "SegmentFilters",
  signature = ".compoundExpr",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    SegmentFilters(do.call("SegmentConditionFilter", exprList), scope = scope)
  }
)

#' @describeIn SegmentFilters
setMethod(
  f = "SegmentFilters",
  signature = ".gaSegmentFilter",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    new("gaSegmentFilterList", exprList, conditionScope = scope)
  }
)

#' @describeIn SegmentFilters
setMethod(
  f = "SegmentFilters",
  signature = "gaSegmentFilterList",
  definition = function(object) {
    object
  }
)

# ---- ScopeLevel, ScopeLevel<- ----

#' @describeIn ScopeLevel
setMethod(
  f = "ScopeLevel",
  signature = "gaSegmentFilterList",
  definition = function(object) {
    object@conditionScope
  }
)

#' @describeIn ScopeLevel
setMethod(
  f = "ScopeLevel<-",
  signature = c("gaSegmentFilterList", "character"),
  definition = function(object, value) {
    object@conditionScope <- value
    validObject(object)
    object
  }
)

# ---- PerSession, PerUser ----

#' @describeIn PerSession
setMethod(
  f = "PerSession",
  signature = "gaSegmentFilterList",
  definition = function(object, ...) {
    ScopeLevel(object) <- "sessions"
    SegmentFilters(object, ...)
  }
)

#' @describeIn PerSession
setMethod(
  f = "PerSession",
  signature = ".gaSegmentFilter",
  definition = function(object, ...) {
    SegmentFilters(object, ..., scope = "sessions")
  }
)

#' @describeIn PerSession
setMethod(
  f = "PerSession",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    SegmentFilters(object, ..., scope = "sessions")
  }
)

#' @describeIn PerSession
setMethod(
  f = "PerSession",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    ScopeLevel(object) <- "perSession"
    if (!missing(...)) {
      SegmentFilters(object, ...)
    }
  }
)

#' @describeIn PerUser
setMethod(
  f = "PerUser",
  signature = "gaSegmentFilterList",
  definition = function(object, ...) {
    ScopeLevel(object) <- "users"
    SegmentFilters(object, ...)
  }
)

#' @describeIn PerUser
setMethod(
  f = "PerUser",
  signature = ".gaSegmentFilter",
  definition = function(object, ...) {
    SegmentFilters(object, ..., scope = "users")
  }
)

#' @describeIn PerUser
setMethod(
  f = "PerUser",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    SegmentFilters(object, ..., scope = "users")
  }
)

#' @describeIn PerUser
setMethod(
  f = "PerUser",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    ScopeLevel(object) <- "perUser"
    if (!missing(...)) {
      SegmentFilters(object, ...)
    }
  }
)

#' @describeIn PerHit
setMethod(
  f = "PerHit",
  signature = "gaMetExpr",
  definition = function(object) {
    ScopeLevel(object) <- "perHit"
    object
  }
)

#' @describeIn PerHit
setMethod(
  f = "PerHit",
  signature = ".compoundExpr",
  definition = function(object, ...) {
    Sequence(And(object, ...))
  }
)

# ---- Segment, Segment<- ----

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "gaSegmentId",
  definition = function(object) {
    object
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "character",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "numeric",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "gaDynSegment",
  definition = function(object) {
    object
  }
)

#' @describeIn Segment
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

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "gaSegmentFilterList",
  definition = function(object, ...) {
    exprList <- list(object, ...)
    new("gaDynSegment", exprList)
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = ".gaSegmentFilter",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr) {
      SegmentFilters(
        expr,
        scope = scope
      )
    })
    do.call(Segment, exprList)
  }
)

#' @describeIn Segment Coerce a Table Filter into a Segment
setMethod(
  f = "Segment",
  signature = "gaFilter",
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

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "NULL",
  definition = function(object) {
    new("gaDynSegment", list())
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment<-",
  signature = c("gaDynSegment", "andExpr"),
  definition = function(object, value) {
    as(object, "andExpr") <- value
    object
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment<-",
  signature = c("gaDynSegment", "ANY"),
  definition = function(object, value) {
    as(value, "gaDynSegment")
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment<-",
  signature = "gaSegmentId",
  definition = function(object, value) {
    to <- class(value)
    as(object, to) <- value
    object
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "gaQuery",
  definition = function(object) {
    object@segments
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment<-",
  signature = c("gaQuery", "ANY"),
  definition = function(object, value) {
    object@segments <- Segment(value)
    validObject(object)
    object
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "gaUserSegment",
  definition = function(object) {
    Segment(object$segmentId)
  }
)

