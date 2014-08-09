# Generic functions
# -----------------
#' @include all-classes.R
#' @include init-methods.R
#' @include ArgList.R
#' @include flatten-list.R
NULL

#' GaVar
#' 
#' Gets or Creates an object from the superclass .gaVar
#' 
#' @export
#' @rdname GaVar
setGeneric(
  name = "GaVar",
  def = function(.Object) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' GaVar<-
#' 
#' Sets the value of an object or sets its slot belonging to the superclass .gaVar
#' 
#' @export
#' @rdname GaVar
setGeneric(
  name = "GaVar<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaVar<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaOperator
#' 
#' Get or create an operator used in an expression.
#' 
#' @export
#' @rdname GaOperator
setGeneric(
  name = "GaOperator",
  def = function(.Object) {},
  valueClass = ".gaOperator",
  useAsDefault = FALSE
)

#' GaOperator<-
#' 
#' Set the operator used in an expression.
#' 
#' @export
#' @rdname GaOperator
setGeneric(
  name = "GaOperator<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaOperator<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaDimOperator
#' 
#' Get or create an operator used specifically in a dimension type expression.
#' 
#' @export
#' @rdname GaDimOperator
setGeneric(
  name = "GaDimOperator",
  def = function(.Object) {},
  valueClass = "gaDimOperator",
  useAsDefault = FALSE
)

#' GaMetOperator
#' 
#' Get or create an operator used specifically in a metric type expression.
#' 
#' @export
#' @rdname GaMetOperator
setGeneric(
  name = "GaMetOperator",
  def = function(.Object) {},
  valueClass = "gaMetOperator",
  useAsDefault = FALSE
)

#' GaOperand
#' 
#' Get the operand of an expression.
#' 
#' @export
#' @rdname GaOperand
setGeneric(
  name = "GaOperand",
  def = function(.Object) {},
  valueClass = ".gaOperand",
  useAsDefault = FALSE
)

#' GaOperand<-
#' 
#' Set the operand of an expression.
#' 
#' @export
#' @rdname GaOperand
setGeneric(
  name = "GaOperand<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaOperand<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaIsRegEx
#' 
#' Checks for a regular expression.
#' 
#' @export
#' @rdname GaIsRegEx
setGeneric(
  name = "GaIsRegEx",
  def = function(.Object) {},
  valueClass = "logical"
)

#' GaNot
#' 
#' NOT an expression.
#' 
#' @export
#' @rdname GaNot
setGeneric(
  name = "GaNot",
  def = function(.Object) {},
  valueClass = c(".gaOperator",".gaCompoundExpr"),
  useAsDefault = FALSE
)

#' GaExpr
#' 
#' Create an expression.
#' 
#' @examples
#' \donttest{
#'   myQuery <- GaQuery(profileId = 123456789)
#'   source_matches_google <- GaExpr("source", "~", "google")
#'   GaFilter(myQuery) <- source_matches_google
#'   GetGaData(myQuery)
#' }
#' 
#' @export
#' @rdname GaExpr
setGeneric(
  name = "GaExpr",
  def = function(.Object, gaOperator, gaOperand, metricScope = "") {},
  valueClass = ".gaExpr",
  useAsDefault = FALSE
)

#' GaOr
#' 
#' OR two or more expressions.
#' 
#' @export
#' @rdname GaOr
setGeneric(
  name = "GaOr",
  def = function(.Object, ...) {},
  valueClass = "gaOr",
  useAsDefault = FALSE
)

#' GaAnd
#' 
#' AND two or more ganalytics expressions together.
#' 
#' Create a new AND expression from one or more arguments
#' Valid types are either AND, OR, or single expressions.
#' A single list of objects is also accepted.
#' 
#' @export
#' @rdname GaAnd
setGeneric(
  name = "GaAnd",
  def = function(.Object, ...) {},
  valueClass = "gaAnd",
  useAsDefault = FALSE
)

#' GaPrecedes
#' 
#' Create a gaSequenceStep object
#' 
#' @export
#' @rdname GaPrecedes
setGeneric(
  name = "GaPrecedes",
  def = function(.Object, ...) {},
  valueClass = "gaSequenceStep",
  useAsDefault = FALSE
)

#' GaImmediatelyPrecedes
#' 
#' Create a gaSequenceStep object
#' 
#' @export
#' @rdname GaImmediatelyPrecedes
setGeneric(
  name = "GaImmediatelyPrecedes",
  def = function(.Object, ...) {},
  valueClass = "gaSequenceStep",
  useAsDefault = FALSE
)

#' GaStartsWith
#' 
#' Create a gaSequenceStep object
#' 
#' @export
#' @rdname GaStartsWith
setGeneric(
  name = "GaStartsWith",
  def = function(.Object, ...) {},
  valueClass = "gaSequenceStep",
  useAsDefault = FALSE
)

#' GaSequenceCondition
#' 
#' Create a new gaSequenceCondition object
#' 
#' @export
#' @rdname GaSequenceCondition
setGeneric(
  name = "GaSequenceCondition",
  def = function(.Object, ...) {},
  valueClass = "gaSequenceCondition",
  useAsDefault = FALSE
)

#' GaNonSequenceCondition
#' 
#' Create a new gaNonSequenceCondition object
#' 
#' @export
#' @rdname GaNonSequenceCondition
setGeneric(
  name = "GaNonSequenceCondition",
  def = function(.Object, ...) {},
  valueClass = "gaNonSequenceCondition",
  useAsDefault = FALSE
)

#' GaSegmentCondition
#' 
#' Create a new gaSegmentCondition object
#' 
#' @export
#' @rdname GaSegmentCondition
setGeneric(
  name = "GaSegmentCondition",
  def = function(.Object, ..., scope = "sessions") {},
  valueClass = "gaSegmentCondition",
  useAsDefault = FALSE
)

#' GaScopeLevel
#' 
#' Get the scope level of a gaDynSegment or gaMetExpr
#' 
#' @export
#' @rdname GaScopeLevel
setGeneric(
  name = "GaScopeLevel",
  def = function(.Object) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' GaScopeLevel<-
#' 
#' Set the scope level of a gaDynSegment or a gaMetExpr
#' For segments, one of 'users' or 'sessions'
#' For metric expressions one of 'perUser', 'perSession' or 'perHit'
#' 
#' @export
#' @rdname GaScopeLevel
setGeneric(
  name = "GaScopeLevel<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaScopeLevel<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaSegment
#' 
#' Get the segment.
#' 
#' @export
#' @rdname GaSegment
setGeneric(
  name = "GaSegment",
  def = function(.Object, ..., scope = "sessions") {},
  valueClass = ".gaSegment",
  useAsDefault = FALSE
)

#' GaSegment<-
#' 
#' Set the segment
#' 
#' @export
#' @rdname GaSegment
setGeneric(
  name = "GaSegment<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaSegment<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaFilter
#' 
#' Get the filter.
#' 
#' @export
#' @rdname GaFilter
setGeneric(
  name = "GaFilter",
  def = function(.Object, ...) {},
  valueClass = "gaFilter",
  useAsDefault = FALSE
)

#' GaFilter<-
#' 
#' Set the filter.
#' 
#' @export
#' @rdname GaFilter
setGeneric(
  name = "GaFilter<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaFilter<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaDateRange
#' 
#' Get the date range.
#' 
#' @export
#' @rdname GaDateRange
setGeneric(
  name = "GaDateRange",
  def =  function(.Object, endDate) {},
  valueClass = "gaDateRange",
  useAsDefault = FALSE
)

#' GaDateRange<-
#' 
#' Set the date range.
#' 
#' @export
#' @rdname GaDateRange
setGeneric(
  name = "GaDateRange<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaDateRange<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaStartDate
#' 
#' Get the start date.
#' 
#' @export
#' @rdname GaStartDate
setGeneric(
  name = "GaStartDate",
  def = function(.Object) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' GaStartDate<-
#' 
#' Set the start date.
#' 
#' @export
#' @rdname GaStartDate
setGeneric(
  name = "GaStartDate<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaStartDate<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaEndDate
#' 
#' Get the end date of the date range.
#' 
#' @export
#' @rdname GaEndDate
setGeneric(
  name = "GaEndDate",
  def = function(.Object) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' GaEndDate<-
#' 
#' Set the endDate of the date range.
#' 
#' @export
#' @rdname GaEndDate
setGeneric(
  name = "GaEndDate<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaEndDate<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaMetrics
#' 
#' Get the metrics of the object.
#' 
#' @export
#' @rdname GaMetrics
setGeneric(
  name = "GaMetrics",
  def = function(.Object, ...) {},
  valueClass = "gaMetrics",
  useAsDefault = FALSE
)

#' GaMetrics<-
#' 
#' Set the metrics of the object.
#' 
#' @export
#' @rdname GaMetrics
setGeneric(
  name = "GaMetrics<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaMetrics<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaDimensions
#' 
#' Get the dimensions of the object.
#' 
#' @export
#' @rdname GaDimensions
setGeneric(
  name = "GaDimensions",
  def = function(.Object, ...) {},
  valueClass = "gaDimensions",
  useAsDefault = FALSE
)

#' GaDimensions<-
#' 
#' Set the dimensions for the object.
#' 
#' @export
#' @rdname GaDimensions
setGeneric(
  name = "GaDimensions<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaDimensions<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaSortBy
#' 
#' Get the sortBy order of the query.
#' 
#' @export
#' @rdname GaSortBy
setGeneric(
  name = "GaSortBy",
  def = function(.Object, ..., desc = logical(0)) {},
  valueClass = "gaSortBy",
  useAsDefault = FALSE
)

#' GaSortBy<-
#' 
#' Set the order of rows returned by Google Analytics.
#' 
#' @export
#' @rdname GaSortBy
setGeneric(
  name = "GaSortBy<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaSortBy<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaProfileId
#' 
#' Get the profileId of the query
#' 
#' @export
#' @rdname GaProfileId
setGeneric(
  name = "GaProfileId",
  def = function(.Object) {},
  valueClass = "gaProfileId",
  useAsDefault = FALSE
)

#' GaProfileId<-
#' 
#' Set the profileId for the query.
#' 
#' @export
#' @rdname GaProfileId
setGeneric(
  name = "GaProfileId<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaProfileId<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaMaxResults
#' 
#' Get the value set for MaxResults.
#' 
#' @export
#' @rdname GaMaxResults
setGeneric(
  name = "GaMaxResults",
  def = function(.Object) {},
  valueClass = "numeric",
  useAsDefault = FALSE
)

#' GaMaxResults<-
#' 
#' Set the maximum rows returned by a ganalytics query.
#' 
#' @export
#' @rdname GaMaxResults
setGeneric(
  name = "GaMaxResults<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaMaxResults<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaSamplingLevel
#' 
#' Get the sampling level.
#' 
#' @export
#' @rdname GaSamplingLevel
setGeneric(
  name = "GaSamplingLevel",
  def = function(.Object) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' GaSamplingLevel<-
#' 
#' Set the sampling level for a ganalytics query.
#' 
#' @export
#' @rdname GaSamplingLevel
setGeneric(
  name = "GaSamplingLevel<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaSamplingLevel<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GetGaUrl
#' 
#' Get the utf8 URL string compoent for the given ganalytics object.
#' 
#' @rdname GetGaUrl
setGeneric(
  name = "GetGaUrl",
  def = function(.Object) {},
  valueClass = "utf8",
  useAsDefault = FALSE
)
