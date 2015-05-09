# Generic functions
# -----------------
#' @include all-classes.R
NULL

#' Var
#'
#' Create, set or get an object of class '.var'.
#'
#' Use Var to lookup a dimension or metric from the Google Analytics core
#' reporting, multi-channel funnel reporting, or real-time reporting APIs, for
#' use in defining expressions (of superclass .expr) or variable lists (of
#' superclass .varList) such as query dimensions, metrics or sortBy parameters.
#' @export
#' @rdname Var
setGeneric(
  "Var",
  function(object, ...) {},
  valueClass = ".var",
  useAsDefault = FALSE
)

#' Var<-
#'
#' Sets the value of an object belonging to the superclass '.var' or sets the
#' var slot of an expression object belonging to class '.expr'
#'
#' @export
#' @rdname Var
setGeneric(
  "Var<-",
  function(object, value) {
    object <- standardGeneric("Var<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("GaVar<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("McfVar<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("RtVar<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("Operator<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("Operand<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object) {},
  valueClass = c(".operator", ".compoundExpr", ".gaSimpleOrSequence"),
  useAsDefault = FALSE
)

#' Expr
#'
#' @export
#' @rdname GaExpr
setGeneric(
  "Expr",
  function(object, operator, operand, metricScope = "") {},
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
  function(object, operator, operand, metricScope = "") {},
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
#'   TableFilter(myQuery) <- source_matches_google
#'   GetData(myQuery)
#' }
#'
#' @export
#' @rdname McfExpr
setGeneric(
  "McfExpr",
  function(object, operator, operand) {},
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
#'   TableFilter(myQuery) <- source_matches_google
#'   GetData(myQuery)
#' }
#'
#' @export
#' @rdname RtExpr
setGeneric(
  "RtExpr",
  function(object, operator, operand) {},
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
  function(object, ...) {},
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
  function(object, ...) {},
  valueClass = "andExpr",
  useAsDefault = FALSE
)

#' @export
setGeneric("xor")

#' GaPrecedes
#'
#' Create a gaSequenceStep object
#'
#' @export
#' @rdname GaPrecedes
setGeneric(
  "GaPrecedes",
  function(object, ...) {},
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
  function(object, ...) {},
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
  function(object, ...) {},
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
  function(object, ..., negation = FALSE) {},
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
  function(object, ..., negation = FALSE) {},
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
  function(object, ..., scope = "sessions") {},
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("GaScopeLevel<-")
    validObject(object)
    object
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
  function(object, ..., scope = "sessions") {},
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
  function(object, value) {
    object <- standardGeneric("GaSegment<-")
    validObject(object)
    object
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
  function(object, ...) {},
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
  function(object, value) {
    object <- standardGeneric("TableFilter<-")
    validObject(object)
    object
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
  function(object, endDate) {},
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
  function(object, value) {
    object <- standardGeneric("DateRange<-")
    validObject(object)
    return(object)
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("StartDate<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("EndDate<-")
    validObject(object)
    object
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
  function(object, ...) {},
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
  function(object, value) {
    object <- standardGeneric("Metrics<-")
    validObject(object)
    object
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
  function(object, ...) {},
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
  function(object, value) {
    object <- standardGeneric("Dimensions<-")
    validObject(object)
    object
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
  function(object, ..., desc = logical(0)) {},
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
  function(object, value) {
    object <- standardGeneric("SortBy<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("GaView<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("MaxResults<-")
    validObject(object)
    object
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
  function(object) {},
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
  function(object, value) {
    object <- standardGeneric("SamplingLevel<-")
    validObject(object)
    object
  }
)

#' GetGaQueries
#'
#' Get the chracter string query compoents for the given ganalytics object.
#'
#' @rdname GetGaUrl
setGeneric(
  "GetGaQueries",
  function(object) {},
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
