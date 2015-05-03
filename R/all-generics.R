# Generic functions
# -----------------
#' @include all-classes.R
NULL

#' Var
#' 
#' @export
setGeneric(
  "Var",
  function(.Object) {},
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
  "Var<-",
  function(.Object, value) {
    .Object <- standardGeneric("Var<-")
    validObject(.Object)
    .Object
  }
)

#' GaVar
#' 
#' Gets or Creates an object from the superclass .gaVar
#' 
#' @export
#' @rdname GaVar
setGeneric(
  "GaVar",
  function(.Object) {},
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
  "GaVar<-",
  function(.Object, value) {
    .Object <- standardGeneric("GaVar<-")
    validObject(.Object)
    .Object
  }
)

#' McfVar
#' 
#' Gets or Creates an object from the superclass .mcfVar
#' 
#' @export
#' @rdname McfVar
setGeneric(
  "McfVar",
  function(.Object) {},
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
  "McfVar<-",
  function(.Object, value) {
    .Object <- standardGeneric("McfVar<-")
    validObject(.Object)
    .Object
  }
)

#' RtVar
#' 
#' Gets or Creates an object from the superclass .rtVar
#' 
#' @export
#' @rdname RtVar
setGeneric(
  "RtVar",
  function(.Object) {},
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
  "RtVar<-",
  function(.Object, value) {
    .Object <- standardGeneric("RtVar<-")
    validObject(.Object)
    .Object
  }
)

#' Operator
#' 
#' Get or create an operator used in an expression.
#' 
#' @export
#' @rdname Operator
setGeneric(
  "Operator",
  function(.Object) {},
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
  "Operator<-",
  function(.Object, value) {
    .Object <- standardGeneric("Operator<-")
    validObject(.Object)
    .Object
  }
)

#' Operand
#' 
#' Get the operand of an expression.
#' 
#' @export
#' @rdname Operand
setGeneric(
  "Operand",
  function(.Object) {},
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
  "Operand<-",
  function(.Object, value) {
    .Object <- standardGeneric("Operand<-")
    validObject(.Object)
    .Object
  }
)

#' IsRegEx
#' 
#' Checks for a regular expression.
#' 
#' @export
#' @rdname IsRegEx
setGeneric(
  "IsRegEx",
  function(.Object) {},
  valueClass = "logical",
  useAsDefault = FALSE
)

#' Not
#' 
#' NOT an expression.
#' 
#' @export
#' @rdname Not
setGeneric(
  "Not",
  function(.Object) {},
  valueClass = c(".operator", ".compoundExpr", ".gaSimpleOrSequence"),
  useAsDefault = FALSE
)

#' Expr
#' 
#' @export
#' @rdname GaExpr
setGeneric(
  "Expr",
  function(.Object, operator, operand, metricScope = "") {},
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
  "GaExpr",
  function(.Object, operator, operand, metricScope = "") {},
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
  "McfExpr",
  function(.Object, operator, operand) {},
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
  "RtExpr",
  function(.Object, operator, operand) {},
  valueClass = ".rtExpr",
  useAsDefault = FALSE
)

#' Or
#' 
#' OR two or more expressions.
#' 
#' @export
#' @rdname Or
setGeneric(
  "Or",
  function(.Object, ...) {},
  valueClass = "orExpr",
  useAsDefault = FALSE
)

#' And
#' 
#' AND two or more ganalytics expressions together.
#' 
#' Create a new AND expression from one or more arguments
#' Valid types are either AND, OR, or single expressions.
#' A single list of objects is also accepted.
#' 
#' @export
#' @rdname And
setGeneric(
  "And",
  function(.Object, ...) {},
  valueClass = "andExpr",
  useAsDefault = FALSE
)

#' GaPrecedes
#' 
#' Create a gaSequenceStep object
#' 
#' @export
#' @rdname GaPrecedes
setGeneric(
  "GaPrecedes",
  function(.Object, ...) {},
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
  "GaImmediatelyPrecedes",
  function(.Object, ...) {},
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
  "GaStartsWith",
  function(.Object, ...) {},
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
  "GaSequenceCondition",
  function(.Object, ..., negation = FALSE) {},
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
  "GaNonSequenceCondition",
  function(.Object, ..., negation = FALSE) {},
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
  "GaSegmentCondition",
  function(.Object, ..., scope = "sessions") {},
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
  "GaScopeLevel",
  function(.Object) {},
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
  "GaScopeLevel<-",
  function(.Object, value) {
    .Object <- standardGeneric("GaScopeLevel<-")
    validObject(.Object)
    .Object
  }
)

#' GaSegment
#' 
#' Get the segment.
#' 
#' @export
#' @rdname GaSegment
setGeneric(
  "GaSegment",
  function(.Object, ..., scope = "sessions") {},
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
  "GaSegment<-",
  function(.Object, value) {
    .Object <- standardGeneric("GaSegment<-")
    validObject(.Object)
    .Object
  }
)

#' TableFilter
#' 
#' Get the filter.
#' 
#' @export
#' @rdname TableFilter
setGeneric(
  "TableFilter",
  function(.Object, ...) {},
  valueClass = ".tableFilter",
  useAsDefault = FALSE
)

#' TableFilter<-
#' 
#' Set the filter.
#' 
#' @export
#' @rdname TableFilter
setGeneric(
  "TableFilter<-",
  function(.Object, value) {
    .Object <- standardGeneric("TableFilter<-")
    validObject(.Object)
    .Object
  }
)

#' GaFilter
#' 
#' Get the filter.
#' 
#' @export
#' @rdname GaFilter
setGeneric(
  "GaFilter",
  function(.Object, ...) {},
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
  "GaFilter<-",
  function(.Object, value) {
    .Object <- standardGeneric("GaFilter<-")
    validObject(.Object)
    .Object
  }
)

#' McfFilter
#' 
#' Get the filter.
#' 
#' @export
#' @rdname McfFilter
setGeneric(
  "McfFilter",
  function(.Object, ...) {},
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
  "McfFilter<-",
  function(.Object, value) {
    .Object <- standardGeneric("McfFilter<-")
    validObject(.Object)
    .Object
  }
)

#' RtFilter
#' 
#' Get the filter.
#' 
#' @export
#' @rdname RtFilter
setGeneric(
  "RtFilter",
  function(.Object, ...) {},
  valueClass = "rtFilter",
  useAsDefault = FALSE
)

#' RtFilter<-
#' 
#' Set the filter.
#' 
#' @export
#' @rdname RtFilter
setGeneric(
  "RtFilter<-",
  function(.Object, value) {
    .Object <- standardGeneric("RtFilter<-")
    validObject(.Object)
    .Object
  }
)

#' DateRange
#' 
#' Get the date range.
#' 
#' @export
#' @rdname DateRange
setGeneric(
  "DateRange",
  function(.Object, endDate) {},
  valueClass = "dateRange",
  useAsDefault = FALSE
)

#' DateRange<-
#' 
#' Set the date range.
#' 
#' @export
#' @rdname DateRange
setGeneric(
  "DateRange<-",
  function(.Object, value) {
    .Object <- standardGeneric("DateRange<-")
    validObject(.Object)
    return(.Object)
  }
)

#' StartDate
#' 
#' Get the start date.
#' 
#' @export
#' @rdname StartDate
setGeneric(
  "StartDate",
  function(.Object) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' StartDate<-
#' 
#' Set the start date.
#' 
#' @export
#' @rdname StartDate
setGeneric(
  "StartDate<-",
  function(.Object, value) {
    .Object <- standardGeneric("StartDate<-")
    validObject(.Object)
    .Object
  }
)

#' EndDate
#' 
#' Get the end date of the date range.
#' 
#' @export
#' @rdname EndDate
setGeneric(
  "EndDate",
  function(.Object) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' EndDate<-
#' 
#' Set the endDate of the date range.
#' 
#' @export
#' @rdname EndDate
setGeneric(
  "EndDate<-",
  function(.Object, value) {
    .Object <- standardGeneric("EndDate<-")
    validObject(.Object)
    .Object
  }
)

#' Metrics
#' 
#' Get the metrics of the object.
#' 
#' @export
#' @rdname Metrics
setGeneric(
  "Metrics",
  function(.Object, ...) {},
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
  "Metrics<-",
  function(.Object, value) {
    .Object <- standardGeneric("Metrics<-")
    validObject(.Object)
    .Object
  }
)

#' Dimensions
#' 
#' Get the dimensions of the object.
#' 
#' @export
#' @rdname Dimensions
setGeneric(
  "Dimensions",
  function(.Object, ...) {},
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
  "Dimensions<-",
  function(.Object, value) {
    .Object <- standardGeneric("Dimensions<-")
    validObject(.Object)
    .Object
  }
)

#' GaMetrics
#' 
#' Get the metrics of the object.
#' 
#' @export
#' @rdname GaMetrics
setGeneric(
  "GaMetrics",
  function(.Object, ...) {},
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
  "GaMetrics<-",
  function(.Object, value) {
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
  "GaDimensions",
  function(.Object, ...) {},
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
  "GaDimensions<-",
  function(.Object, value) {
    .Object <- standardGeneric("GaDimensions<-")
    validObject(.Object)
    .Object
  }
)

#' McfMetrics
#' 
#' Get the metrics of the object.
#' 
#' @export
#' @rdname McfMetrics
setGeneric(
  "McfMetrics",
  function(.Object, ...) {},
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
  "McfMetrics<-",
  function(.Object, value) {
    .Object <- standardGeneric("McfMetrics<-")
    validObject(.Object)
    .Object
  }
)

#' McfDimensions
#' 
#' Get the dimensions of the object.
#' 
#' @export
#' @rdname McfDimensions
setGeneric(
  "McfDimensions",
  function(.Object, ...) {},
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
  "McfDimensions<-",
  function(.Object, value) {
    .Object <- standardGeneric("McfDimensions<-")
    validObject(.Object)
    .Object
  }
)

#' RtMetrics
#' 
#' Get the metrics of the object.
#' 
#' @export
#' @rdname RtMetrics
setGeneric(
  "RtMetrics",
  function(.Object, ...) {},
  valueClass = "rtMetrics",
  useAsDefault = FALSE
)

#' RtMetrics<-
#' 
#' Set the metrics of the object.
#' 
#' @export
#' @rdname RtMetrics
setGeneric(
  "RtMetrics<-",
  function(.Object, value) {
    .Object <- standardGeneric("RtMetrics<-")
    validObject(.Object)
    .Object
  }
)

#' RtDimensions
#' 
#' Get the dimensions of the object.
#' 
#' @export
#' @rdname RtDimensions
setGeneric(
  "RtDimensions",
  function(.Object, ...) {},
  valueClass = "rtDimensions",
  useAsDefault = FALSE
)

#' RtDimensions<-
#' 
#' Set the dimensions for the object.
#' 
#' @export
#' @rdname RtDimensions
setGeneric(
  "RtDimensions<-",
  function(.Object, value) {
    .Object <- standardGeneric("RtDimensions<-")
    validObject(.Object)
    .Object
  }
)

#' SortBy
#' 
#' Get the sortBy order of the query.
#' 
#' @export
#' @rdname SortBy
setGeneric(
  "SortBy",
  function(.Object, ..., desc = logical(0)) {},
  valueClass = c(".sortBy", ".query", "NULL"),
  useAsDefault = FALSE
)

#' SortBy<-
#' 
#' Set the order of rows returned by Google Analytics.
#' 
#' @export
#' @rdname SortBy
setGeneric(
  "SortBy<-",
  function(.Object, value) {
    .Object <- standardGeneric("SortBy<-")
    validObject(.Object)
    .Object
  }
)

setGeneric(
  "GaSortBy",
  function(.Object, ..., desc = logical(0)) {},
  valueClass = c("gaSortBy", ".query", "NULL"),
  useAsDefault = FALSE
)

setGeneric(
  "GaSortBy<-",
  function(.Object, value) {
    .Object <- standardGeneric("GaSortBy<-")
    validObject(.Object)
    .Object
  }
)

setGeneric(
  "McfSortBy",
  function(.Object, ..., desc = logical(0)) {},
  valueClass = c("mcfSortBy", ".query", "NULL"),
  useAsDefault = FALSE
)

setGeneric(
  "McfSortBy<-",
  function(.Object, value) {
    .Object <- standardGeneric("McfSortBy<-")
    validObject(.Object)
    .Object
  }
)

setGeneric(
  "RtSortBy",
  function(.Object, ..., desc = logical(0)) {},
  valueClass = c("rtSortBy", ".query", "NULL"),
  useAsDefault = FALSE
)

setGeneric(
  "RtSortBy<-",
  function(.Object, value) {
    .Object <- standardGeneric("RtSortBy<-")
    validObject(.Object)
    .Object
  }
)

#' GaView
#' 
#' Get the viewId of the query
#' 
#' @export
#' @rdname GaView
setGeneric(
  "GaView",
  function(.Object) {},
  valueClass = "viewId",
  useAsDefault = FALSE
)

#' GaView<-
#' 
#' Set the viewId for the query.
#' 
#' @export
#' @rdname GaView
setGeneric(
  "GaView<-",
  function(.Object, value) {
    .Object <- standardGeneric("GaView<-")
    validObject(.Object)
    .Object
  }
)

#' MaxResults
#' 
#' Get the value set for MaxResults.
#' 
#' @export
#' @rdname MaxResults
setGeneric(
  "MaxResults",
  function(.Object) {},
  valueClass = "numeric",
  useAsDefault = FALSE
)

#' MaxResults<-
#' 
#' Set the maximum rows returned by a ganalytics query.
#' 
#' @export
#' @rdname MaxResults
setGeneric(
  "MaxResults<-",
  function(.Object, value) {
    .Object <- standardGeneric("MaxResults<-")
    validObject(.Object)
    .Object
  }
)

#' SamplingLevel
#' 
#' Get the sampling level.
#' 
#' @export
#' @rdname SamplingLevel
setGeneric(
  "SamplingLevel",
  function(.Object) {},
  valueClass = c("character", "list"),
  useAsDefault = FALSE
)

#' SamplingLevel<-
#' 
#' Set the sampling level for a ganalytics query.
#' 
#' @export
#' @rdname SamplingLevel
setGeneric(
  "SamplingLevel<-",
  function(.Object, value) {
    .Object <- standardGeneric("SamplingLevel<-")
    validObject(.Object)
    .Object
  }
)

#' GetGaQueries
#' 
#' Get the chracter string query compoents for the given ganalytics object.
#' 
#' @rdname GetGaUrl
setGeneric(
  "GetGaQueries",
  function(.Object) {},
  valueClass = "matrix",
  useAsDefault = FALSE
)

#' GetGaData
#' 
#' Fetch the data for the query or object from the Google Analytics API.
#' 
#' @export
#' @rdname GetGaData
setGeneric("GetGaData", function(
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
setGeneric("GaCreds", function(object = "GANALYTICS", creds = NULL, ...) {
  standardGenericric("GaCreds")
})

#' GaCreds<-
#' 
#' Set the authentication credentials for a Google Analytics query object.
#' 
#' @export
#' @rdname GaCreds
setGeneric(
  "GaCreds<-",
  function(object, value) {
    object <- standardGeneric("GaCreds<-")
    validObject(object)
    object
  }
)
