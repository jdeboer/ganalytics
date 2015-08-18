#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
#' @include management-api-classes.R
NULL

# ---- GaPrecedes, GaImmediatelyPrecedes, GaStartsWith, GaSequence ----

#' @describeIn GaSequence
setMethod(
  f = "GaPrecedes",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSequenceStep", as(object, "andExpr"), immediatelyPrecedes = FALSE)
  }
)

#' @describeIn GaSequence
setMethod(
  f = "GaImmediatelyPrecedes",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSequenceStep", as(object, "andExpr"), immediatelyPrecedes = TRUE)
  }
)

#' @describeIn GaSequence
setMethod(
  f = "GaStartsWith",
  signature = ".compoundExpr",
  definition = function(object) {
    GaImmediatelyPrecedes(object)
  }
)

#' @describeIn GaSequence
setMethod(
  f = "GaSequence",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSequenceStep")})
    new("gaSequenceFilter", exprList, negation = negation)
  }
)

#' @describeIn GaSequence
setMethod(
  f = "GaSequence",
  signature = "gaSequenceStep",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSequenceStep")})
    new("gaSequenceFilter", exprList, negation = negation)
  }
)

# ---- GaNonSequenceCondition, GaSegmentCondition ----

#' @describeIn GaNonSequenceCondition
setMethod(
  f = "GaNonSequenceCondition",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- do.call("And", lapply(exprList, function(expr){as(expr, "andExpr")}))
    new("gaConditionFilter", exprList, negation = negation)
  }
)

#' @describeIn GaSegmentCondition
setMethod(
  f = "GaSegmentCondition",
  signature = ".compoundExpr",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    GaSegmentCondition(do.call(GaNonSequenceCondition, exprList), scope = scope)
  }
)

#' @describeIn GaSegmentCondition
setMethod(
  f = "GaSegmentCondition",
  signature = ".gaSegmentFilter",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    new("gaSegmentFilterList", exprList, conditionScope = scope)
  }
)

#' @describeIn GaSegmentCondition
setMethod(
  f = "GaSegmentCondition",
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
      GaSegmentCondition(
        GaNonSequenceCondition(expr),
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
      GaSegmentCondition(
        expr,
        scope = scope
      )
    })
    do.call(GaSegment, exprList)
  }
)

#' @describeIn Segment Coerce a Table Filter into a Segment
setMethod(
  f = "Segment",
  signature = "gaFilter",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr) {
      GaSegmentCondition(
        GaNonSequenceCondition(expr),
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
    object@segments <- GaSegment(value)
    validObject(object)
    object
  }
)

#' @describeIn Segment
setMethod(
  f = "Segment",
  signature = "gaUserSegment",
  definition = function(object) {
    GaSegment(object$segmentId)
  }
)

# Backwards compatibility
#' GaSegment (Deprecated).
#'
#' @param ... arguments passed onto \code{Segment}
#' @export GaSegment
#' @rdname GaSegment
GaSegment <- function(...){Segment(...)}

#' GaSegment<- (Deprecated).
#'
#' @param value passed onto \code{Segment}
#' @export GaSegment<-
#' @rdname GaSegment
`GaSegment<-` <- function(..., value){`Segment<-`(..., value)}
