#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
NULL

# ---- GaPrecedes, GaImmediatelyPrecedes, GaStartsWith, GaSequenceCondition ----

setMethod(
  f = "GaPrecedes",
  signature = ".gaCompoundExpr",
  definition = function(.Object) {
    new("gaSequenceStep", as(.Object, "gaAnd"), immediatelyPrecedes = FALSE)
  }
)

setMethod(
  f = "GaImmediatelyPrecedes",
  signature = ".gaCompoundExpr",
  definition = function(.Object) {
    new("gaSequenceStep", as(.Object, "gaAnd"), immediatelyPrecedes = TRUE)
  }
)

setMethod(
  f = "GaStartsWith",
  signature = ".gaCompoundExpr",
  definition = function(.Object) {
    GaImmediatelyPrecedes(.Object)
  }
)

setMethod(
  f = "GaSequenceCondition",
  signature = "gaSequenceStep",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    new("gaSequenceCondition", exprList)
  }
)

# ---- GaNonSequenceCondition, GaSegmentCondition ----

setMethod(
  f = "GaNonSequenceCondition",
  signature = ".gaCompoundExpr",
  definition = function(.Object) {
    as(.Object, "gaNonSequenceCondition")
  }
)

setMethod(
  f = "GaSegmentCondition",
  signature = ".gaDimensionOrMetricConditions",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    new("gaSegmentCondition", exprList)
  }
)

# ---- GaSegment, GaSegment<- ----

setMethod(
  f = "GaSegment",
  signature = "gaSegmentId",
  definition = function(.Object) {
    return(.Object)
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
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    new("gaDynSegment", exprList)
  }
)

setMethod(
  f = "GaSegment",
  signature = ".gaCompoundExpr",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    exprList <- lapply(exprList, function(expr) {
      GaSegmentCondition(
        GaNonSequenceCondition(expr)
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
  signature = "gaFilter",
  definition = function(.Object, ...) {
    exprList <- list(.Object, ...)
    exprList <- lapply(exprList, function(expr) {
      GaSegmentCondition(
        GaNonSequenceCondition(expr)
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
  signature = c("gaDynSegment", "gaAnd"),
  definition = function(.Object, value) {
    as(.Object, "gaAnd") <- value
    return(.Object)
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
    return(.Object)
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
    return(.Object)
  }
)
