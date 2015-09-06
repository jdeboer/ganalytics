#' @include segment-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include management-api-classes.R
#' @importFrom methods new setMethod
NULL

# ---- Later, Then, First, GaSequence ----

#' @describeIn GaSequence
setMethod(
  f = "Later",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSegmentSequenceStep", as(object, "andExpr"), immediatelyPrecedes = FALSE)
  }
)

#' @describeIn GaSequence
setMethod(
  f = "Then",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSegmentSequenceStep", as(object, "andExpr"), immediatelyPrecedes = TRUE)
  }
)

#' @describeIn GaSequence
setMethod(
  f = "First",
  signature = ".compoundExpr",
  definition = function(object) {
    Then(object)
  }
)

#' @describeIn GaSequence
setMethod(
  f = "GaSequence",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSegmentSequenceStep")})
    new("gaSegmentSequenceFilter", exprList, negation = negation)
  }
)

#' @describeIn GaSequence
setMethod(
  f = "GaSequence",
  signature = "gaSegmentSequenceStep",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSegmentSequenceStep")})
    new("gaSegmentSequenceFilter", exprList, negation = negation)
  }
)

# ---- GaCondition, GaSegmentFilters ----

#' @describeIn GaCondition
setMethod(
  f = "GaCondition",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- do.call("And", lapply(exprList, function(expr){as(expr, "andExpr")}))
    new("gaSegmentConditionFilter", exprList, negation = negation)
  }
)

#' @describeIn GaSegmentFilters
setMethod(
  f = "GaSegmentFilters",
  signature = ".compoundExpr",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    GaSegmentFilters(do.call(GaCondition, exprList), scope = scope)
  }
)

#' @describeIn GaSegmentFilters
setMethod(
  f = "GaSegmentFilters",
  signature = ".gaSegmentFilter",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    new("gaSegmentFilterList", exprList, conditionScope = scope)
  }
)

#' @describeIn GaSegmentFilters
setMethod(
  f = "GaSegmentFilters",
  signature = "gaSegmentFilterList",
  definition = function(object) {
    object
  }
)

# ---- GaScopeLevel, GaScopeLevel<- ----

#' @describeIn GaScopeLevel
setMethod(
  f = "GaScopeLevel",
  signature = "gaSegmentFilterList",
  definition = function(object) {
    object@conditionScope
  }
)

#' @describeIn GaScopeLevel
setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaSegmentFilterList", "character"),
  definition = function(object, value) {
    object@conditionScope <- value
    object
  }
)

# ---- GaPerSession, GaPerUser ----

#' @describeIn GaPerSession
setMethod(
  f = "GaPerSession",
  signature = "gaSegmentFilterList",
  definition = function(object, ...) {
    GaScopeLevel(object) <- "sessions"
    GaSegmentFilters(object, ...)
  }
)

#' @describeIn GaPerSession
setMethod(
  f = "GaPerSession",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    GaScopeLevel(object) <- "perSession"
    if (!missing(...)) {
      GaSegmentFilters(object, ...)
    }
  }
)

#' @describeIn GaPerUser
setMethod(
  f = "GaPerUser",
  signature = "gaSegmentFilterList",
  definition = function(object, ...) {
    GaScopeLevel(object) <- "users"
    GaSegmentFilters(object, ...)
  }
)

#' @describeIn GaPerUser
setMethod(
  f = "GaPerUser",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    GaScopeLevel(object) <- "perUser"
    if (!missing(...)) {
      GaSegmentFilters(object, ...)
    }
  }
)

#' @describeIn GaPerHit
setMethod(
  f = "GaPerHit",
  signature = "gaMetExpr",
  definition = function(object, ...) {
    GaScopeLevel(object) <- "perHit"
    if (!missing(...)) {
      GaSegmentFilters(object, ...)
    }
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
      GaSegmentFilters(
        GaCondition(expr),
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
      GaSegmentFilters(
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
      GaSegmentFilters(
        GaCondition(expr),
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

