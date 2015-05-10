# Generic functions
# -----------------
#' @include all-classes.R
NULL

#' Google Analytics dimension and metric variables.
#'
#' \code{Var} returns a '.var' object which is valid Google Analytics dimension
#' or metric for use with the core reporting, multi-channel-funnel reporting or
#' real-time reporting API.
#'
#' Use \code{Var} to lookup a dimension or metric from the Google Analytics core
#' reporting, multi-channel-funnel reporting, or real-time reporting APIs, for
#' use in defining expressions (of superclass '.expr') or (to be implemented)
#' variable lists (of superclass '.varList') such as query dimensions, metrics
#' or sortBy parameters.
#'
#' \code{Var} accepts either a character, '.var', or '.expr' object. A character
#' object will be coerced to a '.var' object by looking for a matching dimension
#' or metric from the Core Reporting, Multi-Channel Funnel Reporting, and
#' Real-Time Reporting APIs. Providing an '.expr' object will return the
#' dimension or metric used within that Google Analytics expression.
#'
#' @param object an object that inherits from or extends the class '.var',
#'   including 'gaDimVar', 'gaMetVar', 'mcfDimVar', 'mcfMetVar', 'rtDimVar',
#'   'rtMetVar', 'gaExpr', 'mcfExpr', 'rtExpr', 'gaDimensions', 'gaMetrics',
#'   'mcfDimensions', 'mcfMetrics', 'rtDimensions' and 'rtMetrics'.
#' @param ... A replacement value for \code{object} coerced to class '.var'.
#'
#' @return An object inheriting from the superclass '.var'
#'
#' @examples
#' Var("source")
#' dim <- Var("ga:medium")
#' Var(dim)
#' paid_traffic <- Expr(dim, "==", "cpc")
#' Var(paid_traffic)
#'
#' @seealso \itemize{ \item
#'   \href{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}{Core
#'    Reporting API dimensions and metrics} \item
#'   \href{https://developers.google.com/analytics/devguides/reporting/realtime/dimsmets/}{Multi-Channel-Funnel
#'    Reporting API dimensions and metrics} \item
#'   \href{https://developers.google.com/analytics/devguides/reporting/mcf/dimsmets/}{Real-Time
#'    Reporting API dimensions and metrics} }
#'
#' @export
#' @rdname Var
setGeneric(
  "Var",
  function(object, ...) {},
  valueClass = ".var",
  useAsDefault = FALSE
)

#' Var<-.
#'
#' \code{Var<-} sets the value of an object belonging to the superclass '.var'
#' or sets the var slot of an expression object belonging to class '.expr'
#'
#' @param value any object that can be coerced to a valid \code{object} class.
#'
#' @examples
#' expr1 <- Expr("pageviews", '>', 10)
#' Var(expr1) <- "uniquePageviews"
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

#' \code{GaVar} Gets or creates an object from the superclass .gaVar
#'
#' @export
#' @rdname Var
setGeneric(
  "GaVar",
  function(object, ...) {},
  valueClass = ".gaVar",
  useAsDefault = FALSE
)

#' \code{GaVar<-} replaces a '.var' object or sets the var slot of '.expr'
#' object to a Google Analytics Core Reporting API dimension or metric
#' that inherits from the class '.gaVar'.
#'
#' @export
#' @rdname Var
setGeneric(
  "GaVar<-",
  function(object, value) {
    object <- standardGeneric("GaVar<-")
    validObject(object)
    object
  }
)

#' \code{McfVar} Gets or creates an object from the superclass .mcfVar
#'
#' @export
#' @rdname Var
setGeneric(
  "McfVar",
  function(object, ...) {},
  valueClass = ".mcfVar",
  useAsDefault = FALSE
)

#' \code{McfVar<-} replaces a '.var' object or sets the var slot of '.expr'
#' object to a Multi Channel Funnel Reporting API dimension or metric
#' that inherits from the class '.mcfVar'.
#'
#' @export
#' @rdname Var
setGeneric(
  "McfVar<-",
  function(object, value) {
    object <- standardGeneric("McfVar<-")
    validObject(object)
    object
  }
)

#' \code{RtVar} Gets or creates an object from the superclass .rtVar
#'
#' @export
#' @rdname Var
setGeneric(
  "RtVar",
  function(object, ...) {},
  valueClass = ".rtVar",
  useAsDefault = FALSE
)

#' \code{RtVar<-} replaces a '.var' object or sets the var slot of '.expr'
#' object to a Google Analytics Real Time Reporting API dimension or metric
#' that inherits from the class '.rtVar'.
#'
#' @export
#' @rdname Var
setGeneric(
  "RtVar<-",
  function(object, value) {
    object <- standardGeneric("RtVar<-")
    validObject(object)
    object
  }
)

#' Operator.
#'
#' Get or create an operator used in an expression.
#'
#' @param object The object to be coerced to a '.Operator' class or to get the
#'   operator from.
#' @param ... Used by certain methods.
#'
#' @export
#' @rdname Expr
setGeneric(
  "Operator",
  function(object, ...) {},
  valueClass = ".operator",
  useAsDefault = FALSE
)

#' Operator<-.
#'
#' Set the operator used in an expression.
#'
#' @param object The object for which to set the operator of.
#' @param value The value to set the operator to.
#'
#' @export
#' @rdname Expr
setGeneric(
  "Operator<-",
  function(object, value) {
    object <- standardGeneric("Operator<-")
    validObject(object)
    object
  }
)

#' Operand.
#'
#' Get the operand of an expression.
#'
#' @param object The object for which to set the operand of.
#' @param value The value to set the operand to.
#'
#' @export
#' @rdname Expr
setGeneric(
  "Operand",
  function(object, ...) {},
  valueClass = ".operand",
  useAsDefault = FALSE
)

#' Operand<-.
#'
#' Set the operand of an expression.
#'
#' @param object An object to set the operand of.
#' @param value The value to set the operand to.
#'
#' @export
#' @rdname Expr
setGeneric(
  "Operand<-",
  function(object, value) {
    object <- standardGeneric("Operand<-")
    validObject(object)
    object
  }
)

#' IsRegEx.
#'
#' Checks for a regular expression.
#'
#' @param object An object to check if whether a regular expression.
#'
#' @return TRUE or FALSE
#'
#' @export
#' @rdname Expr
setGeneric(
  "IsRegEx",
  function(object) {},
  valueClass = "logical",
  useAsDefault = FALSE
)

#' Expr.
#'
#' Define a Google Analytics expression.
#'
#' @param object A dimension or metric variable, or another object to be coerced
#'   to an .expr object.
#'
#' @export
#' @rdname Expr
setGeneric(
  "Expr",
  function(object, operator, operand, metricScope = "") {},
  valueClass = ".expr",
  useAsDefault = FALSE
)

#' GaExpr.
#'
#' Create a Core Reporting API expression.
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
#' @rdname Expr
setGeneric(
  "GaExpr",
  function(object, operator, operand, metricScope = "") {},
  valueClass = ".gaExpr",
  useAsDefault = FALSE
)

#' McfExpr.
#'
#' Create a Multi-Chanel Funnel Reporting API expression.
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
#' @rdname Expr
setGeneric(
  "McfExpr",
  function(object, operator, operand) {},
  valueClass = ".mcfExpr",
  useAsDefault = FALSE
)

#' RtExpr.
#'
#' Create a Real-Time Reporting API expression.
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
#' @rdname Expr
setGeneric(
  "RtExpr",
  function(object, operator, operand) {},
  valueClass = ".rtExpr",
  useAsDefault = FALSE
)

#' Not.
#'
#' Invert an expression, i.e. NOT.
#'
#' @param object An object to get the logical inverse of.
#'
#' @export
#' @rdname Not
setGeneric(
  "Not",
  function(object) {},
  valueClass = c(".operator", ".compoundExpr", ".gaSimpleOrSequence"),
  useAsDefault = FALSE
)

#' Or.
#'
#' OR two or more expressions.
#'
#' @param object The first object to include within the OR expression.
#' @param ... Additional objects to include within the OR expression.
#'
#' @export
#' @rdname Or
setGeneric(
  "Or",
  function(object, ...) {},
  valueClass = "orExpr",
  useAsDefault = FALSE
)

#' And.
#'
#' AND two or more ganalytics expressions together.
#'
#' Create a new AND expression from one or more arguments
#' Valid types are either AND, OR, or single expressions.
#' A single list of objects is also accepted.
#'
#' @param object The first object within the AND expression
#' @param ... Additional objects to include within the AND expression.
#'
#' @export
#' @rdname And
setGeneric(
  "And",
  function(object, ...) {},
  valueClass = "andExpr",
  useAsDefault = FALSE
)

#' Generate an expression that gives the exclusive or of two expressions.
#'
#' @param x The first expression
#' @param y The second expression.
#'
#' @export
#' @rdname Or
setGeneric("xor")

#' GaPrecedes.
#'
#' Create a gaSequenceStep object
#'
#' @export
#' @rdname GaSequenceCondition
setGeneric(
  "GaPrecedes",
  function(object, ...) {},
  valueClass = "gaSequenceStep",
  useAsDefault = FALSE
)

#' GaImmediatelyPrecedes.
#'
#' Create a gaSequenceStep object
#'
#' @export
#' @rdname GaSequenceCondition
setGeneric(
  "GaImmediatelyPrecedes",
  function(object, ...) {},
  valueClass = "gaSequenceStep",
  useAsDefault = FALSE
)

#' GaStartsWith.
#'
#' Create a gaSequenceStep object
#'
#' @export
#' @rdname GaSequenceCondition
setGeneric(
  "GaStartsWith",
  function(object, ...) {},
  valueClass = "gaSequenceStep",
  useAsDefault = FALSE
)

#' GaSequenceCondition.
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

#' GaNonSequenceCondition.
#'
#' Create a new gaNonSequenceCondition object
#'
#' @export
#' @rdname Segment
setGeneric(
  "GaNonSequenceCondition",
  function(object, ..., negation = FALSE) {},
  valueClass = "gaNonSequenceCondition",
  useAsDefault = FALSE
)

#' GaSegmentCondition.
#'
#' Create a new gaSegmentCondition object
#'
#' @export
#' @rdname Segment
setGeneric(
  "GaSegmentCondition",
  function(object, ..., scope = "sessions") {},
  valueClass = "gaSegmentCondition",
  useAsDefault = FALSE
)

#' GaScopeLevel.
#'
#' Get the scope level of a gaDynSegment or gaMetExpr
#'
#' @export
#' @rdname Segment
setGeneric(
  "GaScopeLevel",
  function(object, ...) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' GaScopeLevel<-.
#'
#' Set the scope level of a gaDynSegment or a gaMetExpr
#' For segments, one of 'users' or 'sessions'
#' For metric expressions one of 'perUser', 'perSession' or 'perHit'
#'
#' @export
#' @rdname Segment
setGeneric(
  "GaScopeLevel<-",
  function(object, value) {
    object <- standardGeneric("GaScopeLevel<-")
    validObject(object)
    object
  }
)

#' GaSegment.
#'
#' Get the segment.
#'
#' @export
#' @rdname Segment
setGeneric(
  "Segment",
  function(object, ..., scope = "sessions") {},
  valueClass = ".gaSegment",
  useAsDefault = FALSE
)

#' Segment<-.
#'
#' Set the segment
#'
#' @export
#' @rdname Segment
setGeneric(
  "Segment<-",
  function(object, value) {
    object <- standardGeneric("Segment<-")
    validObject(object)
    object
  }
)

#' TableFilter.
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

#' TableFilter<-.
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

#' DateRange.
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

#' DateRange<-.
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

#' StartDate.
#'
#' Get the start date.
#'
#' @export
#' @rdname DateRange
setGeneric(
  "StartDate",
  function(object, ...) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' StartDate<-.
#'
#' Set the start date.
#'
#' @export
#' @rdname DateRange
setGeneric(
  "StartDate<-",
  function(object, value) {
    object <- standardGeneric("StartDate<-")
    validObject(object)
    object
  }
)

#' EndDate.
#'
#' Get the end date of the date range.
#'
#' @export
#' @rdname DateRange
setGeneric(
  "EndDate",
  function(object, ...) {},
  valueClass = "Date",
  useAsDefault = FALSE
)

#' EndDate<-.
#'
#' Set the endDate of the date range.
#'
#' @export
#' @rdname DateRange
setGeneric(
  "EndDate<-",
  function(object, value) {
    object <- standardGeneric("EndDate<-")
    validObject(object)
    object
  }
)

#' Metrics.
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

#' Metrics<-.
#'
#' Set the metrics of the object.
#'
#' @rdname Metrics
#' @export
setGeneric(
  "Metrics<-",
  function(object, value) {
    object <- standardGeneric("Metrics<-")
    validObject(object)
    object
  }
)

#' Dimensions.
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

#' Dimensions<-.
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

#' SortBy.
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

#' SortBy<-.
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

#' GaView.
#'
#' Get the viewId of the query
#'
#' @export
#' @rdname GaView
setGeneric(
  "GaView",
  function(object, ...) {},
  valueClass = "viewId",
  useAsDefault = FALSE
)

#' GaView<-.
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

#' MaxResults.
#'
#' Get the value set for MaxResults.
#'
#' @export
#' @rdname MaxResults
setGeneric(
  "MaxResults",
  function(object, ...) {},
  valueClass = "numeric",
  useAsDefault = FALSE
)

#' MaxResults<-.
#'
#' Set the maximum rows returned by a ganalytics query.
#'
#' @param object the object to set the maximum response rows of.
#' @param value the value to set the maximum response rows to.
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

#' SamplingLevel.
#'
#' Get the sampling level.
#'
#' @param object the query or response to check the sampling level of.
#' @param value if \code{object} is a query, then use  value to set the sampling
#'   level to of that query.
#'
#' @export
#' @rdname SamplingLevel
setGeneric(
  "SamplingLevel",
  function(object, value) {},
  valueClass = c("character", "list"),
  useAsDefault = FALSE
)

#' SamplingLevel<-.
#'
#' Set the sampling level for a ganalytics query.
#'
#' @param object the query to set the sampling level of.
#' @param value the sampling level to set to.
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

#' GetGaQueries.
#'
#' Get the chracter string query compoents for the given ganalytics object.
setGeneric(
  "GetGaQueries",
  function(object) {},
  valueClass = "matrix",
  useAsDefault = FALSE
)

#' GetGaData.
#'
#' Fetch the data for the Google Analytics API query.
#'
#' @export
#' @rdname Query
setGeneric(
  "GetGaData", function(query, creds = NULL, ...) {
    standardGeneric("GetGaData")
  }
)

#' Authentication credentials for Google Analytics API queries.
#'
#' Get or set the authentication credentials for a Google Analytics query object.
#'
#' @export
#' @rdname GaCreds
setGeneric(
  "GaCreds",
  function(object = "GANALYTICS", creds = NULL, ...) {
    standardGenericric("GaCreds")
  }
)

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
