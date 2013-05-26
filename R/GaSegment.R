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
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaSegment",
  signature = ".gaCompoundExpr",
  definition = function(.Object) {
    as(.Object, "gaDynSegment")
  }
)

setMethod(
  f = "GaSegment",
  signature = "gaFilter",
  definition = function(.Object) {
    as(object = .Object, Class = "gaDynSegment")
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
