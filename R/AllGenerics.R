#' @rdname AllGenerics
#' Generic functions
#' All generic functions defined by ganalytics
#' This file lists all of the generic methods available from ganalytics
#' @include AllClasses.R

#' @rdname GaIsRegEx
#' GaIsRegEx
#' Checks for a regular expression.
#' @export
GaIsRegEx <- function(.Object) {}
setGeneric(
  name = "GaIsRegEx",
  valueClass = "logical"
)

#' @rdname GaVar
#' GaVar
#' Gets or Creates an object from the superclass .gaVar
#' @export
GaVar <- function(.Object) {}
setGeneric(
  name = "GaVar",
  valueClass = "character",
  useAsDefault = FALSE
)

#' @rdname GaVar
#' GaVar<-
#' Sets the value of an object or sets its slot belonging to the superclass .gaVar
#' @export
`GaVar<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaVar<-")
  validObject(.Object)
  return(.Object)
}
setGeneric("GaVar<-")

#' @rdname GaOperator
#' GaOperator
#' Get or create an operator used in an expression.
#' @export
GaOperator <- function(.Object) {}
setGeneric(
  name = "GaOperator",
  valueClass = ".gaOperator",
  useAsDefault = FALSE
)

#' @rdname GaOperator
#' GaOperator<-
#' Set the operator used in an expression.
#' @export
`GaOperator<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaOperator<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaOperator<-"
)

#' @rdname GaDimOperator
#' GaDimOperator
#' Get or create an operator used specifically in a dimension type expression.
#' @export
GaDimOperator <- function(.Object) {}
setGeneric(
  name = "GaDimOperator",
  valueClass = "gaDimOperator",
  useAsDefault = FALSE
)

#' @rdname GaMetOperator
#' GaMetOperator
#' Get or create an operator used specifically in a metric type expression.
#' @export
GaMetOperator <- function(.Object) {}
setGeneric(
  name = "GaMetOperator",
  valueClass = "gaMetOperator",
  useAsDefault = FALSE
)

#' @rdname GaOperand
#' GaOperand
#' Get the operand of an expression.
#' @export
GaOperand <- function(.Object) {}
setGeneric(
  name = "GaOperand",
  valueClass = ".gaOperand",
  useAsDefault = FALSE
)

#' @rdname GaOperand
#' GaOperand<-
#' Set the operand of an expression.
#' @export
`GaOperand<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaOperand<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaOperand<-"
)

#' @rdname GaNot
#' GaNot
#' NOT an expression.
#' @export
GaNot <- function(.Object) {}
setGeneric(
  name = "GaNot",
  valueClass = ".gaLogical",
  useAsDefault = FALSE
)

#' @rdname GaExpr
#' GaExpr
#' Create an expression.
#' @export
GaExpr <- function(.Object, gaOperator, gaOperand) {}
setGeneric(
  name = "GaExpr",
  valueClass = ".gaExpr",
  useAsDefault = FALSE
)

#' @rdname GaOr
#' GaOr
#' OR two or more expressions.
#' @export
GaOr <- function(.Object, ...) {}
setGeneric(
  name = "GaOr",
  valueClass = "gaOr",
  useAsDefault = FALSE
)

#' @rdname GaAnd
#' GaAnd
#' AND two or more expressions.
#' @export
GaAnd <- function(.Object, ...) {}
setGeneric(
  name = "GaAnd",
  valueClass = "gaAnd",
  useAsDefault = FALSE
)

#' @rdname GaSegment
#' GaSegment
#' Get the segment.
#' @export
GaSegment <- function(.Object) {}
setGeneric(
  name = "GaSegment",
  valueClass = ".gaSegment",
  useAsDefault = FALSE
)

#' @rdname GaSegment
#' GaSegment<-
#' Set the segment
#' @export
`GaSegment<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaSegment<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaSegment<-",
)

#' @rdname GaFilter
#' GaFilter
#' Get the filter.
#' @export
GaFilter <- function(.Object, ...) {}
setGeneric(
  name = "GaFilter",
  valueClass = "gaFilter",
  useAsDefault = FALSE
)

#' @rdname GaFilter
#' GaFilter<-
#' Set the filter.
#' @export
`GaFilter<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaFilter<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaFilter<-"
)

#' @rdname GaDateRange
#' GaDateRange
#' Get the date range.
#' @export
GaDateRange <- function(.Object, endDate) {}
setGeneric(
  name = "GaDateRange",
  valueClass = "gaDateRange",
  useAsDefault = FALSE
)

#' @rdname GaDateRange
#' GaDateRange<-
#' Set the date range.
#' @export
`GaDateRange<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaDateRange<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaDateRange<-"
)

#' @rdname GaStartDate
#' GaStartDate
#' Get the start date.
#' @export
GaStartDate <- function(.Object) {}
setGeneric(
  name = "GaStartDate",
  valueClass = "Date",
  useAsDefault = FALSE
)

#' @rdname GaStartDate
#' GaStartDate<-
#' Set the start date.
#' @export
`GaStartDate<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaStartDate<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaStartDate<-"
)

#' @rdname GaEndDate
#' GaEndDate
#' Get the end date of the date range.
#' @export
GaEndDate <- function(.Object) {}
setGeneric(
  name = "GaEndDate",
  valueClass = "Date",
  useAsDefault = FALSE
)

#' @rdname GaEndDate
#' GaEndDate<-
#' Set the endDate of the date range.
#' @export
`GaEndDate<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaEndDate<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaEndDate<-"
)

#' @rdname GaMetrics
#' GaMetrics
#' Get the metrics of the object.
#' @export
GaMetrics <- function(.Object, ...) {}
setGeneric(
  name = "GaMetrics",
  valueClass = "gaMetrics",
  useAsDefault = FALSE
)

#' @rdname GaMetrics
#' GaMetrics<-
#' Set the metrics of the object.
#' @export
`GaMetrics<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaMetrics<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaMetrics<-"
)

#' @rdname GaDimensions
#' @title GaDimensions
#' Get the dimensions of the object.
#' @export
GaDimensions <- function(.Object, ...) {}
setGeneric(
  name = "GaDimensions",
  valueClass = "gaDimensions",
  useAsDefault = FALSE
)

#' @rdname GaDimensions
#' GaDimensions<-
#' Set the dimensions for the object.
#' @export
`GaDimensions<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaDimensions<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaDimensions<-"
)

#' @rdname GaSortBy
#' GaSortBy
#' Get the sortBy order of the query.
#' @export
GaSortBy <- function(.Object, ..., desc = logical(0)) {}
setGeneric(
  name = "GaSortBy",
  valueClass = "gaSortBy",
  useAsDefault = FALSE
)

#' @rdname GaSortBy
#' GaSortBy<-
#' Set the order of rows returned by Google Analytics.
#' @export
`GaSortBy<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaSortBy<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaSortBy<-"
)

#' @rdname GaProfileId
#' GaProfileId
#' Get the profileId of the query
#' @export
GaProfileId <- function(.Object) {}
setGeneric(
  name = "GaProfileId",
  valueClass = "gaProfileId",
  useAsDefault = FALSE
)

#' @rdname GaProfileId
#' GaProfileId<-
#' Set the profileId for the query.
#' @export
`GaProfileId<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaProfileId<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaProfileId<-"
)

#' @rdname GaMaxResults
#' GaMaxResults
#' Get the value set for MaxResults.
#' @export
GaMaxResults <- function(.Object) {}
setGeneric(
  name = "GaMaxResults",
  valueClass = "numeric",
  useAsDefault = FALSE
)

#' @rdname GaMaxResults
#' GaMaxResults<-
#' Set the maximum rows returned by a ganalytics query.
#' @export
`GaMaxResults<-` <- function(.Object, value) {
  .Object <- standardGeneric("GaMaxResults<-")
  validObject(.Object)
  return(.Object)
}
setGeneric(
  name = "GaMaxResults<-"
)

#' @rdname GetGaUrl
#' GetGaUrl
#' Get the utf8 URL string compoent for the given ganalytics object.
#' @export
GetGaUrl <- function(.Object) {}
setGeneric(
  name = "GetGaUrl",
  valueClass = "utf8",
  useAsDefault = FALSE
)
