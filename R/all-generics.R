#' @include all-classes.R
#' @include init-methods.R
#' @include ArgList.R
NULL

# Generic functions
# -----------------
# This file lists all of the generic functions available in the ganalytics package.

#' @rdname GaVar
#' GaVar
#' Gets or Creates an object from the superclass .gaVar
#' @export
#' @genericMethods
GaVar <- 
setGeneric(
  name = "GaVar",
  def = function(.Object) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' @rdname GaVar
#' GaVar<-
#' Sets the value of an object or sets its slot belonging to the superclass .gaVar
#' @export
#' @genericMethods
setGeneric(
  name = "GaVar<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaVar<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaOperator
#' GaOperator
#' Get or create an operator used in an expression.
#' @export
#' @genericMethods
setGeneric(
  name = "GaOperator",
  def = function(.Object) {},
  valueClass = ".gaOperator",
  useAsDefault = FALSE
)

#' @rdname GaOperator
#' GaOperator<-
#' Set the operator used in an expression.
#' @export
#' @genericMethods
setGeneric(
  name = "GaOperator<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaOperator<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaDimOperator
#' GaDimOperator
#' Get or create an operator used specifically in a dimension type expression.
#' @export
#' @genericMethods
setGeneric(
  name = "GaDimOperator",
  def = function(.Object) {},
  valueClass = "gaDimOperator",
  useAsDefault = FALSE
)

#' @rdname GaMetOperator
#' GaMetOperator
#' Get or create an operator used specifically in a metric type expression.
#' @export
#' @genericMethods
setGeneric(
  name = "GaMetOperator",
  def = function(.Object) {},
  valueClass = "gaMetOperator",
  useAsDefault = FALSE
)

#' @rdname GaOperand
#' GaOperand
#' Get the operand of an expression.
#' @export
#' @genericMethods
setGeneric(
  name = "GaOperand",
  def = function(.Object) {},
  valueClass = ".gaOperand",
  useAsDefault = FALSE
)

#' @rdname GaOperand
#' GaOperand<-
#' Set the operand of an expression.
#' @export
#' @genericMethods
setGeneric(
  name = "GaOperand<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaOperand<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaIsRegEx
#' GaIsRegEx
#' Checks for a regular expression.
#' @export
#' @genericMethods
setGeneric(
  name = "GaIsRegEx",
  def = function(.Object) {},
  valueClass = "logical"
)

#' @rdname GaNot
#' GaNot
#' NOT an expression.
#' @export
#' @genericMethods
setGeneric(
  name = "GaNot",
  def = function(.Object) {},
  valueClass = ".gaLogical",
  useAsDefault = FALSE
)

#' @rdname GaExpr
#' GaExpr
#' Create an expression.
#' @examples
#' \donttest{
#'   myQuery <- GaQuery(profileId = 123456789)
#'   source_matches_google <- GaExpr("source", "~", "google")
#'   GaFilter(myQuery) <- source_matches_google
#'   GetGaData(myQuery)
#' }
#' @export
#' @genericMethods
setGeneric(
  name = "GaExpr",
  def = function(.Object, gaOperator, gaOperand) {},
  valueClass = ".gaExpr",
  useAsDefault = FALSE
)

#' @rdname GaOr
#' GaOr
#' OR two or more expressions.
#' @export
#' @genericMethods
setGeneric(
  name = "GaOr",
  def = function(.Object, ...) {},
  valueClass = "gaOr",
  useAsDefault = FALSE
)

#' @rdname GaAnd
#' GaAnd
#' 
#' AND two or more ganalytics expressions together.
#' 
#' Create a new AND expression from one or more arguments
#' Valid types are either AND, OR, or single expressions.
#' A single list of objects is also accepted.
#' 
#' @export
#' @genericMethods
setGeneric(
  name = "GaAnd",
  def = function(.Object, ...) {},
  valueClass = "gaAnd",
  useAsDefault = FALSE
)

#' @rdname GaSegment
#' GaSegment
#' Get the segment.
#' @export
#' @genericMethods
setGeneric(
  name = "GaSegment",
  def = function(.Object) {},
  valueClass = ".gaSegment",
  useAsDefault = FALSE
)

#' @rdname GaSegment
#' GaSegment<-
#' Set the segment
#' @export
#' @genericMethods
setGeneric(
  name = "GaSegment<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaSegment<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaFilter
#' GaFilter
#' Get the filter.
#' @export
#' @genericMethods
setGeneric(
  name = "GaFilter",
  def = function(.Object, ...) {},
  valueClass = "gaFilter",
  useAsDefault = FALSE
)

#' @rdname GaFilter
#' GaFilter<-
#' Set the filter.
#' @export
#' @genericMethods
setGeneric(
  name = "GaFilter<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaFilter<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaDateRange
#' GaDateRange
#' Get the date range.
#' @export
#' @genericMethods
setGeneric(
  name = "GaDateRange",
  def =  function(.Object, endDate) {},
  valueClass = "gaDateRange",
  useAsDefault = FALSE
)

#' @rdname GaDateRange
#' GaDateRange<-
#' Set the date range.
#' @export
#' @genericMethods
setGeneric(
  name = "GaDateRange<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaDateRange<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaStartDate
#' GaStartDate
#' Get the start date.
#' @export
#' @genericMethods
setGeneric(
  name = "GaStartDate",
  def = function(.Object) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' @rdname GaStartDate
#' GaStartDate<-
#' Set the start date.
#' @export
#' @genericMethods
setGeneric(
  name = "GaStartDate<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaStartDate<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaEndDate
#' GaEndDate
#' Get the end date of the date range.
#' @export
#' @genericMethods
setGeneric(
  name = "GaEndDate",
  def = function(.Object) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' @rdname GaEndDate
#' GaEndDate<-
#' Set the endDate of the date range.
#' @export
#' @genericMethods
setGeneric(
  name = "GaEndDate<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaEndDate<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaMetrics
#' GaMetrics
#' Get the metrics of the object.
#' @export
#' @genericMethods
setGeneric(
  name = "GaMetrics",
  def = function(.Object, ...) {},
  valueClass = "gaMetrics",
  useAsDefault = FALSE
)

#' @rdname GaMetrics
#' GaMetrics<-
#' Set the metrics of the object.
#' @export
#' @genericMethods
setGeneric(
  name = "GaMetrics<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaMetrics<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaDimensions
#' GaDimensions
#' Get the dimensions of the object.
#' @export
#' @genericMethods
setGeneric(
  name = "GaDimensions",
  def = function(.Object, ...) {},
  valueClass = "gaDimensions",
  useAsDefault = FALSE
)

#' @rdname GaDimensions
#' GaDimensions<-
#' Set the dimensions for the object.
#' @export
#' @genericMethods
setGeneric(
  name = "GaDimensions<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaDimensions<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaSortBy
#' GaSortBy
#' Get the sortBy order of the query.
#' @export
#' @genericMethods
setGeneric(
  name = "GaSortBy",
  def = function(.Object, ..., desc = logical(0)) {},
  valueClass = "gaSortBy",
  useAsDefault = FALSE
)

#' @rdname GaSortBy
#' GaSortBy<-
#' Set the order of rows returned by Google Analytics.
#' @export
#' @genericMethods
setGeneric(
  name = "GaSortBy<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaSortBy<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaProfileId
#' GaProfileId
#' Get the profileId of the query
#' @export
#' @genericMethods
setGeneric(
  name = "GaProfileId",
  def = function(.Object) {},
  valueClass = "gaProfileId",
  useAsDefault = FALSE
)

#' @rdname GaProfileId
#' GaProfileId<-
#' Set the profileId for the query.
#' @export
#' @genericMethods
setGeneric(
  name = "GaProfileId<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaProfileId<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GaMaxResults
#' GaMaxResults
#' Get the value set for MaxResults.
#' @export
#' @genericMethods
setGeneric(
  name = "GaMaxResults",
  def = function(.Object) {},
  valueClass = "numeric",
  useAsDefault = FALSE
)

#' @rdname GaMaxResults
#' GaMaxResults<-
#' Set the maximum rows returned by a ganalytics query.
#' @export
#' @genericMethods
setGeneric(
  name = "GaMaxResults<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaMaxResults<-")
    validObject(.Object)
    return(.Object)
  }
)

#' @rdname GetGaUrl
#' GetGaUrl
#' Get the utf8 URL string compoent for the given ganalytics object.
#' @export
#' @genericMethods
setGeneric(
  name = "GetGaUrl",
  def = function(.Object) {},
  valueClass = "utf8",
  useAsDefault = FALSE
)
