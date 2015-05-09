#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
#' @include management-api-classes.R
NULL

# ---- GaPrecedes, GaImmediatelyPrecedes, GaStartsWith, GaSequenceCondition ----

setMethod(
  f = "GaPrecedes",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSequenceStep", as(object, "andExpr"), immediatelyPrecedes = FALSE)
  }
)

setMethod(
  f = "GaImmediatelyPrecedes",
  signature = ".compoundExpr",
  definition = function(object) {
    new("gaSequenceStep", as(object, "andExpr"), immediatelyPrecedes = TRUE)
  }
)

setMethod(
  f = "GaStartsWith",
  signature = ".compoundExpr",
  definition = function(object) {
    GaImmediatelyPrecedes(object)
  }
)

setMethod(
  f = "GaSequenceCondition",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSequenceStep")})
    new("gaSequenceCondition", exprList, negation = negation)
  }
)

setMethod(
  f = "GaSequenceCondition",
  signature = "gaSequenceStep",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSequenceStep")})
    new("gaSequenceCondition", exprList, negation = negation)
  }
)

# ---- GaNonSequenceCondition, GaSegmentCondition ----

setMethod(
  f = "GaNonSequenceCondition",
  signature = ".compoundExpr",
  definition = function(object, ..., negation) {
    exprList <- list(object, ...)
    exprList <- do.call("And", lapply(exprList, function(expr){as(expr, "andExpr")}))
    new("gaNonSequenceCondition", exprList, negation = negation)
  }
)

setMethod(
  f = "GaSegmentCondition",
  signature = ".compoundExpr",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    GaSegmentCondition(do.call(GaNonSequenceCondition, exprList), scope = scope)
  }
)

setMethod(
  f = "GaSegmentCondition",
  signature = ".gaSimpleOrSequence",
  definition = function(object, ..., scope) {
    exprList <- list(object, ...)
    new("gaSegmentCondition", exprList, conditionScope = scope)
  }
)

setMethod(
  f = "GaSegmentCondition",
  signature = "gaSegmentCondition",
  definition = function(object) {
    object
  }
)

# ---- GaScopeLevel, GaScopeLevel<- ----

setMethod(
  f = "GaScopeLevel",
  signature = "gaSegmentCondition",
  definition = function(object) {
    object@conditionScope
  }
)

setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaSegmentCondition", "character"),
  definition = function(object, value) {
    object@conditionScope <- value
    object
  }
)

# ---- GaSegment, GaSegment<- ----

setMethod(
  f = "GaSegment",
  signature = "gaSegmentId",
  definition = function(object) {
    object
  }
)

setMethod(
  f = "GaSegment",
  signature = "character",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

setMethod(
  f = "GaSegment",
  signature = "numeric",
  definition = function(object) {
    as(object, "gaSegmentId")
  }
)

setMethod(
  f = "GaSegment",
  signature = "gaDynSegment",
  definition = function(object) {
    object
  }
)

setMethod(
  f = "GaSegment",
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

setMethod(
  f = "GaSegment",
  signature = "gaSegmentCondition",
  definition = function(object, ...) {
    exprList <- list(object, ...)
    new("gaDynSegment", exprList)
  }
)

setMethod(
  f = "GaSegment",
  signature = ".gaSimpleOrSequence",
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

setMethod(
  f = "GaSegment",
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

setMethod(
  f = "GaSegment",
  signature = "NULL",
  definition = function(object) {
    new("gaDynSegment", list())
  }
)

setMethod(
  f = "GaSegment<-",
  signature = c("gaDynSegment", "andExpr"),
  definition = function(object, value) {
    as(object, "andExpr") <- value
    object
  }
)

setMethod(
  f = "GaSegment<-",
  signature = c("gaDynSegment", "ANY"),
  definition = function(object, value) {
    as(value, "gaDynSegment")
  }
)

setMethod(
  f = "GaSegment<-",
  signature = "gaSegmentId",
  definition = function(object, value) {
    to <- class(value)
    as(object, to) <- value
    object
  }
)

setMethod(
  f = "GaSegment",
  signature = "gaQuery",
  definition = function(object) {
    object@segment
  }
)

setMethod(
  f = "GaSegment<-",
  signature = c("gaQuery", "ANY"),
  definition = function(object, value) {
    object@segment <- GaSegment(value)
    validObject(object)
    object
  }
)

setMethod(
  f = "GaSegment",
  signature = "gaUserSegment",
  definition = function(object) {
    GaSegment(object$segmentId)
  }
)

# Forwards compatibility
#'@export Segment
Segment <- GaSegment
#'@export Segment<-
`Segment<-` <- `GaSegment<-`
