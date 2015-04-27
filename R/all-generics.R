# Generic functions
# -----------------
#' @include all-classes.R
NULL

#' Var
#' 
#' @export
setGeneric(
  name = "Var",
  def = function(.Object) {},
  valueClass = ".var",
  useAsDefault = FALSE
)

#' Var<-
#' 
#' Sets the value of an object or sets its slot belonging to the superclass .mcfVar
#' 
#' @export
#' @rdname McfVar
setGeneric(
  name = "Var<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("Var<-")
    validObject(.Object)
    return(.Object)
  }
)

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

#' McfVar
#' 
#' Gets or Creates an object from the superclass .mcfVar
#' 
#' @export
#' @rdname McfVar
setGeneric(
  name = "McfVar",
  def = function(.Object) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' McfVar<-
#' 
#' Sets the value of an object or sets its slot belonging to the superclass .mcfVar
#' 
#' @export
#' @rdname McfVar
setGeneric(
  name = "McfVar<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("McfVar<-")
    validObject(.Object)
    return(.Object)
  }
)

#' RtVar
#' 
#' Gets or Creates an object from the superclass .rtVar
#' 
#' @export
#' @rdname RtVar
setGeneric(
  name = "RtVar",
  def = function(.Object) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' RtVar<-
#' 
#' Sets the value of an object or sets its slot belonging to the superclass .rtVar
#' 
#' @export
#' @rdname RtVar
setGeneric(
  name = "RtVar<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("RtVar<-")
    validObject(.Object)
    return(.Object)
  }
)

#' Operator
#' 
#' Get or create an operator used in an expression.
#' 
#' @export
#' @rdname Operator
setGeneric(
  name = "Operator",
  def = function(.Object) {},
  valueClass = ".operator",
  useAsDefault = FALSE
)

#' Operator<-
#' 
#' Set the operator used in an expression.
#' 
#' @export
#' @rdname Operator
setGeneric(
  name = "Operator<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("Operator<-")
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
  valueClass = ".dimOperator",
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
  valueClass = ".metOperator",
  useAsDefault = FALSE
)

#' McfDimOperator
#' 
#' Get or create an operator used specifically in a dimension type expression.
#' 
#' @export
#' @rdname McfDimOperator
setGeneric(
  name = "McfDimOperator",
  def = function(.Object) {},
  valueClass = ".dimOperator",
  useAsDefault = FALSE
)

#' McfMetOperator
#' 
#' Get or create an operator used specifically in a metric type expression.
#' 
#' @export
#' @rdname McfMetOperator
setGeneric(
  name = "McfMetOperator",
  def = function(.Object) {},
  valueClass = ".metOperator",
  useAsDefault = FALSE
)

#' RtDimOperator
#' 
#' Get or create an operator used specifically in a dimension type expression.
#' 
#' @export
#' @rdname RtDimOperator
setGeneric(
  name = "RtDimOperator",
  def = function(.Object) {},
  valueClass = ".dimOperator",
  useAsDefault = FALSE
)

#' RtMetOperator
#' 
#' Get or create an operator used specifically in a metric type expression.
#' 
#' @export
#' @rdname RtMetOperator
setGeneric(
  name = "RtMetOperator",
  def = function(.Object) {},
  valueClass = ".metOperator",
  useAsDefault = FALSE
)

#' Operand
#' 
#' Get the operand of an expression.
#' 
#' @export
#' @rdname Operand
setGeneric(
  name = "Operand",
  def = function(.Object) {},
  valueClass = ".operand",
  useAsDefault = FALSE
)

#' Operand<-
#' 
#' Set the operand of an expression.
#' 
#' @export
#' @rdname Operand
setGeneric(
  name = "Operand<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("Operand<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaOperand
#' 
#' Get the operand of an expression.
#' 
#' @export
#' @rdname Operand
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

#' McfOperand
#' 
#' Get the operand of an expression.
#' 
#' @export
#' @rdname McfOperand
setGeneric(
  name = "McfOperand",
  def = function(.Object) {},
  valueClass = ".mcfOperand",
  useAsDefault = FALSE
)

#' McfOperand<-
#' 
#' Set the operand of an expression.
#' 
#' @export
#' @rdname McfOperand
setGeneric(
  name = "McfOperand<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("McfOperand<-")
    validObject(.Object)
    return(.Object)
  }
)

#' RtOperand
#' 
#' Get the operand of an expression.
#' 
#' @export
#' @rdname RtOperand
setGeneric(
  name = "RtOperand",
  def = function(.Object) {},
  valueClass = ".rtOperand",
  useAsDefault = FALSE
)

#' RtOperand<-
#' 
#' Set the operand of an expression.
#' 
#' @export
#' @rdname RtOperand
setGeneric(
  name = "RtOperand<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("RtOperand<-")
    validObject(.Object)
    return(.Object)
  }
)

#' IsRegEx
#' 
#' Checks for a regular expression.
#' 
#' @export
#' @rdname IsRegEx
setGeneric(
  name = "IsRegEx",
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
  valueClass = c(".operator", ".compoundExpr", ".gaSimpleOrSequence"),
  useAsDefault = FALSE
)

#' Expr
#' 
setGeneric(
  name = "Expr",
  def = function(.Object, operator, operand, metricScope = "") {},
  valueClass = ".expr",
  useAsDefault = FALSE
)

#' GaExpr
#' 
#' Create an expression.
#' 
#' @examples
#' \donttest{
#'   myQuery <- GaQuery(view = 123456789)
#'   source_matches_google <- GaExpr("source", "~", "google")
#'   GaFilter(myQuery) <- source_matches_google
#'   GetGaData(myQuery)
#' }
#' 
#' @export
#' @rdname GaExpr
setGeneric(
  name = "GaExpr",
  def = function(.Object, operator, operand, metricScope = "") {},
  valueClass = ".gaExpr",
  useAsDefault = FALSE
)

#' McfExpr
#' 
#' Create an expression.
#' 
#' @examples
#' \donttest{
#'   myQuery <- McfQuery(view = 123456789)
#'   source_matches_google <- McfExpr("mcf:source", "~", "google")
#'   Filter(myQuery) <- source_matches_google
#'   GetData(myQuery)
#' }
#' 
#' @export
#' @rdname McfExpr
setGeneric(
  name = "McfExpr",
  def = function(.Object, operator, operand) {},
  valueClass = ".mcfExpr",
  useAsDefault = FALSE
)

#' RtExpr
#' 
#' Create an expression.
#' 
#' @examples
#' \donttest{
#'   myQuery <- RtQuery(view = 123456789)
#'   source_matches_google <- RtExpr("rt:source", "~", "google")
#'   Filter(myQuery) <- source_matches_google
#'   GetData(myQuery)
#' }
#' 
#' @export
#' @rdname RtExpr
setGeneric(
  name = "RtExpr",
  def = function(.Object, operator, operand) {},
  valueClass = ".rtExpr",
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
  def = function(.Object, ..., negation = FALSE) {},
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
  def = function(.Object, ..., negation = FALSE) {},
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

#' TableFilter
#' 
#' Get the filter.
#' 
#' @export
#' @rdname TableFilter
setGeneric(
  name = "TableFilter",
  def = function(.Object, ...) {},
  valueClass = ".filter",
  useAsDefault = FALSE
)

#' TableFilter<-
#' 
#' Set the filter.
#' 
#' @export
#' @rdname TableFilter
setGeneric(
  name = "TableFilter<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("TableFilter<-")
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

#' McfFilter
#' 
#' Get the filter.
#' 
#' @export
#' @rdname McfFilter
setGeneric(
  name = "McfFilter",
  def = function(.Object, ...) {},
  valueClass = "mcfFilter",
  useAsDefault = FALSE
)

#' McfFilter<-
#' 
#' Set the filter.
#' 
#' @export
#' @rdname McfFilter
setGeneric(
  name = "McfFilter<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("McfFilter<-")
    validObject(.Object)
    return(.Object)
  }
)

#' RtFilter
#' 
#' Get the filter.
#' 
#' @export
#' @rdname RtFilter
setGeneric(
  name = "RtFilter",
  def = function(.Object, ...) {},
  valueClass = "RtFilter",
  useAsDefault = FALSE
)

#' RtFilter<-
#' 
#' Set the filter.
#' 
#' @export
#' @rdname RtFilter
setGeneric(
  name = "RtFilter<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("RtFilter<-")
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

#' Metrics
#' 
#' Get the metrics of the object.
#' 
#' @export
#' @rdname Metrics
setGeneric(
  name = "Metrics",
  def = function(.Object, ...) {},
  valueClass = ".metrics",
  useAsDefault = FALSE
)

#' Metrics<-
#' 
#' Set the metrics of the object.
#' 
#' @export
#' @rdname Metrics
setGeneric(
  name = "Metrics<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("Metrics<-")
    validObject(.Object)
    return(.Object)
  }
)

#' Dimensions
#' 
#' Get the dimensions of the object.
#' 
#' @export
#' @rdname Dimensions
setGeneric(
  name = "Dimensions",
  def = function(.Object, ...) {},
  valueClass = ".dimensions",
  useAsDefault = FALSE
)

#' Dimensions<-
#' 
#' Set the dimensions for the object.
#' 
#' @export
#' @rdname Dimensions
setGeneric(
  name = "Dimensions<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("Dimensions<-")
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



#' McfMetrics
#' 
#' Get the metrics of the object.
#' 
#' @export
#' @rdname McfMetrics
setGeneric(
  name = "McfMetrics",
  def = function(.Object, ...) {},
  valueClass = "mcfMetrics",
  useAsDefault = FALSE
)

#' McfMetrics<-
#' 
#' Set the metrics of the object.
#' 
#' @export
#' @rdname McfMetrics
setGeneric(
  name = "McfMetrics<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("McfMetrics<-")
    validObject(.Object)
    return(.Object)
  }
)

#' McfDimensions
#' 
#' Get the dimensions of the object.
#' 
#' @export
#' @rdname McfDimensions
setGeneric(
  name = "McfDimensions",
  def = function(.Object, ...) {},
  valueClass = "mcfDimensions",
  useAsDefault = FALSE
)

#' McfDimensions<-
#' 
#' Set the dimensions for the object.
#' 
#' @export
#' @rdname McfDimensions
setGeneric(
  name = "McfDimensions<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("McfDimensions<-")
    validObject(.Object)
    return(.Object)
  }
)

#' SortBy
#' 
#' Get the sortBy order of the query.
#' 
#' @export
#' @rdname SortBy
setGeneric(
  name = "SortBy",
  def = function(.Object, ..., desc = logical(0)) {},
  valueClass = c(".sortBy", "gaQuery"),
  useAsDefault = FALSE
)

#' SortBy<-
#' 
#' Set the order of rows returned by Google Analytics.
#' 
#' @export
#' @rdname SortBy
setGeneric(
  name = "SortBy<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("SortBy<-")
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
  valueClass = c("gaSortBy", "gaQuery"),
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

#' McfSortBy
#' 
#' Get the sortBy order of the query.
#' 
#' @export
#' @rdname McfSortBy
setGeneric(
  name = "McfSortBy",
  def = function(.Object, ..., desc = logical(0)) {},
  valueClass = c("mcfSortBy", "gaQuery"),
  useAsDefault = FALSE
)

#' McfSortBy<-
#' 
#' Set the order of rows returned by Google Analytics.
#' 
#' @export
#' @rdname McfSortBy
setGeneric(
  name = "McfSortBy<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("McfSortBy<-")
    validObject(.Object)
    return(.Object)
  }
)

#' GaView
#' 
#' Get the viewId of the query
#' 
#' @export
#' @rdname GaView
setGeneric(
  name = "GaView",
  def = function(.Object) {},
  valueClass = "gaProfileId",
  useAsDefault = FALSE
)

#' GaView<-
#' 
#' Set the viewId for the query.
#' 
#' @export
#' @rdname GaView
setGeneric(
  name = "GaView<-",
  def = function(.Object, value) {
    .Object <- standardGeneric("GaView<-")
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
  valueClass = c("character", "list"),
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

#' GetGaQueries
#' 
#' Get the chracter string query compoents for the given ganalytics object.
#' 
#' @rdname GetGaUrl
setGeneric(
  name = "GetGaQueries",
  def = function(.Object) {},
  valueClass = "matrix",
  useAsDefault = FALSE
)

#' GetGaData
#' 
#' Fetch the data for the query or object from the Google Analytics API.
#' 
#' @export
#' @rdname GetGaData
setGeneric("GetGaData", def = function(
  object, 
  creds = NULL,
  ...
) {
  standardGeneric("GetGaData")
})

#' GaCreds
#' 
#' Get or set the authentication credentials for a Google Analytics query object.
#' 
#' @export
#' @rdname GaCreds
setGeneric("GaCreds", def = function(object = "GANALYTICS", creds = NULL, ...) {
  standardGenericric("GaCreds")
})

#' GaCreds<-
#' 
#' Set the authentication credentials for a Google Analytics query object.
#' 
#' @export
#' @rdname GaCreds
setGeneric(
  name = "GaCreds<-",
  def = function(object, value) {
    object <- standardGeneric("GaCreds<-")
    validObject(object)
    return(object)
  }
)
