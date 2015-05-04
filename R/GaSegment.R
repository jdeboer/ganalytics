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
  definition = function(.Object) {
    new("gaSequenceStep", as(.Object, "andExpr"), immediatelyPrecedes = FALSE)
  }
)

setMethod(
  f = "GaImmediatelyPrecedes",
  signature = ".compoundExpr",
  definition = function(.Object) {
    new("gaSequenceStep", as(.Object, "andExpr"), immediatelyPrecedes = TRUE)
  }
)

setMethod(
  f = "GaStartsWith",
  signature = ".compoundExpr",
  definition = function(.Object) {
    GaImmediatelyPrecedes(.Object)
  }
)

setMethod(
  f = "GaSequenceCondition",
  signature = ".compoundExpr",
  definition = function(.Object, ..., negation) {
    exprList <- list(.Object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSequenceStep")})
    new("gaSequenceCondition", exprList, negation = negation)
  }
)

setMethod(
  f = "GaSequenceCondition",
  signature = "gaSequenceStep",
  definition = function(.Object, ..., negation) {
    exprList <- list(.Object, ...)
    exprList <- lapply(exprList, function(expr){as(expr, "gaSequenceStep")})
    new("gaSequenceCondition", exprList, negation = negation)
  }
)

# ---- GaNonSequenceCondition, GaSegmentCondition ----

setMethod(
  f = "GaNonSequenceCondition",
  signature = ".compoundExpr",
  definition = function(.Object, ..., negation) {
    exprList <- list(.Object, ...)
    exprList <- do.call("And", lapply(exprList, function(expr){as(expr, "andExpr")}))
    new("gaNonSequenceCondition", exprList, negation = negation)
  }
)

setMethod(
  f = "GaSegmentCondition",
  signature = ".compoundExpr",
  definition = function(.Object, ..., scope) {
    exprList <- list(.Object, ...)
    GaSegmentCondition(do.call(GaNonSequenceCondition, exprList), scope = scope)
  }
)

setMethod(
  f = "GaSegmentCondition",
  signature = ".gaSimpleOrSequence",
  definition = function(.Object, ..., scope) {
    exprList <- list(.Object, ...)
    new("gaSegmentCondition", exprList, conditionScope = scope)
  }
)

setMethod(
  f = "GaSegmentCondition",
  signature = "gaSegmentCondition",
  definition = function(.Object) {
    .Object
  }
)

# ---- GaScopeLevel, GaScopeLevel<- ----

setMethod(
  f = "GaScopeLevel",
  signature = "gaSegmentCondition",
  definition = function(.Object) {
    .Object@conditionScope
  }
)

setMethod(
  f = "GaScopeLevel<-",
  signature = c("gaSegmentCondition", "character"),
  definition = function(.Object, value) {
    .Object@conditionScope <- value
    .Object
  }
)

# ---- GaSegment, GaSegment<- ----

setMethod(
  f = "GaSegment",
  signature = "gaSegmentId",
  definition = function(.Object) {
    .Object
  }
)

setMethod(
  f = "GaSegment",
  signature = "character",
  definition = function(.Object) {
    as(.Object, "gaSegmentId")
  }
)

setMethod(
  f = "GaSegment",
  signature = "numeric",
  definition = function(.Object) {
    as(.Object, "gaSegmentId")
  }
)

setMethod(
  f = "GaSegment",
  signature = "gaDynSegment",
  definition = function(.Object) {
    .Object
  }
)

setMethod(
  f = "GaSegment",
  signature = ".compoundExpr",
  definition = function(.Object, ..., scope) {
    exprList <- list(.Object, ...)
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
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    new("gaDynSegment", exprList)
  }
)

setMethod(
  f = "GaSegment",
  signature = ".gaSimpleOrSequence",
  definition = function(.Object, ..., scope) {
    exprList <- list(.Object, ...)
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
  definition = function(.Object, ..., scope) {
    exprList <- list(.Object, ...)
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
  definition = function(.Object) {
    new("gaDynSegment", list())
  }
)

setMethod(
  f = "GaSegment<-",
  signature = c("gaDynSegment", "andExpr"),
  definition = function(.Object, value) {
    as(.Object, "andExpr") <- value
    .Object
  }
)

setMethod(
  f = "GaSegment<-",
  signature = c("gaDynSegment", "ANY"),
  definition = function(.Object, value) {
    as(value, "gaDynSegment")
  }
)

setMethod(
  f = "GaSegment<-",
  signature = "gaSegmentId",
  definition = function(.Object, value) {
    to <- class(value)
    as(.Object, to) <- value
    .Object
  }
)

setMethod(
  f = "GaSegment",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@segment
  }
)

setMethod(
  f = "GaSegment<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@segment <- GaSegment(value)
    validObject(.Object)
    .Object
  }
)

setMethod(
  f = "GaSegment",
  signature = "gaUserSegment",
  definition = function(.Object) {
    GaSegment(.Object$segmentId)
  }
)

# Forwards compatibility
#'@export Segment
Segment <- GaSegment
#'@export Segment<-
`Segment<-` <- `GaSegment<-`
