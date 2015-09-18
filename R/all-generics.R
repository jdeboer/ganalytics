# Generic functions
# -----------------

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
#' or sets the var slot of an expression object belonging to superclass '.expr'
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

#' Comparator.
#'
#' Get or create an comparator used in an expression.
#'
#' @param object The object to be coerced to a '.Comparator' class or to get the
#'   comparator from.
#' @param ... Used by certain methods.
#'
#' @export
#' @rdname Comparator
setGeneric(
  "Comparator",
  function(object, ...) {},
  valueClass = ".comparator",
  useAsDefault = FALSE
)

#' Comparator<-.
#'
#' Set the comparator used in an expression.
#'
#' @param value The value to set the comparator to.
#'
#' @export
#' @rdname Comparator
setGeneric(
  "Comparator<-",
  function(object, value) {
    object <- standardGeneric("Comparator<-")
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
#' @rdname Operand
setGeneric(
  "Operand",
  function(object, value) {},
  valueClass = ".operand",
  useAsDefault = FALSE
)

#' Operand<-.
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

#' IsRegEx.
#'
#' Checks for a regular expression.
#'
#' @param object An object to check if whether a regular expression.
#'
#' @return TRUE or FALSE
#'
#' @export
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
#' @param comparator The comparator to use for the expression.
#' @param operand The operand to use for the expression.
#' @param metricScope The scope to use for segmentation if using a metric.
#'   Possible values include "perUser" or "perSession".
#'
#' @export
setGeneric(
  "Expr",
  function(object, comparator, operand, metricScope = "") {},
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
#' @inheritParams Expr
#' @export
setGeneric(
  "GaExpr",
  function(object, comparator, operand, metricScope = "") {},
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
#' @inheritParams Expr
#' @export
setGeneric(
  "McfExpr",
  function(object, comparator, operand) {},
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
#' @inheritParams Expr
#' @export
setGeneric(
  "RtExpr",
  function(object, comparator, operand) {},
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
  valueClass = c(".comparator", ".compoundExpr", ".gaSegmentFilter"),
  useAsDefault = FALSE
)

#' Or.
#'
#' OR two or more expressions.
#'
#' @param object The first object to include within the OR expression.
#' @param ... Additional objects to include within the OR expression.
#'
#' @return An object of class orExpr.
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
#' @return an object of class \code{andExpr}
#'
#' @export
#' @rdname And
setGeneric(
  "And",
  function(object, ...) {},
  valueClass = "andExpr",
  useAsDefault = FALSE
)

#' Generate an expression that gives the Exclusive-OR of two expressions.
#'
#' @param x,y Conditions for an exclusive-or expression.
#'
#'
#' @export
#' @rdname xor
setGeneric("xor")

#' Later.
#'
#' Treat a step within a sequence as happening at any point after any preceding
#' steps in the sequence, i.e. 'later'. 'Later' means 'followed by', but not
#' necessarily immediately.
#'
#' @param object The expression that should preceed others in the sequence.
#' @param ... Any other expressions that should follow the first one but before
#'   any others in the sequence.
#'
#' @return a gaSegmentSequenceStep object, with the immediate flag not set.
#'
#' @seealso Sequence
#'
#' @export
setGeneric(
  "Later",
  function(object, ...) {},
  valueClass = "gaSegmentSequenceStep",
  useAsDefault = FALSE
)

#' Then.
#'
#' Treat a step within a sequence as happening immediately after any preceding
#' steps in the sequence, i.e. 'immediately following'.
#'
#' @param object The expression that should \bold{immediately} preceed others in
#'   the sequence.
#' @param ... Any other expressions that should \bold{immediately} follow the
#'   first one but before any others in the sequence.
#'
#' @return a gaSegmentSequenceStep object, with the immediate flag set.
#'
#' @export
#' @seealso Sequence
setGeneric(
  "Then",
  function(object, ...) {},
  valueClass = "gaSegmentSequenceStep",
  useAsDefault = FALSE
)

#' First.
#'
#' If used at the beginning of a sequence, indicates that this step must match
#' the first interaction of included sessions and users within the select date
#' range. First expressly means 'first interaction' within the date range.
#'
#' @param object An expression that should be at the start of a sequence
#'   expression.
#' @param ... Any other expressions that should immediately follow the first
#'   expression.
#'
#' @return a gaSegmentSequenceStep object, with the immediate flag set.
#'
#'
#' @export
#' @seealso Sequence
setGeneric(
  "First",
  function(object, ...) {},
  valueClass = "gaSegmentSequenceStep",
  useAsDefault = FALSE
)

#' Sequence.
#'
#' Create a new gaSequence object
#'
#' @param object A sequence step or another expression that should be coerced to
#'   a sequence condition.
#' @param ... Other steps within the sequence condition, in the order in which
#'   they should be applied.
#' @param negation Logical TRUE or FALSE to match segments where this sequence
#'   has not occured.
#'
#' @export
setGeneric(
  "Sequence",
  function(object, ..., negation = FALSE) {
    if (!missing(negation)) {
      warning("Argument 'negation' is deprecated. Instead, please wrap the sequence or condtion within an Include or Exclude call.")
    }
    standardGeneric("Sequence")
  },
  valueClass = "gaSegmentSequenceFilter",
  useAsDefault = FALSE
)

#' SegmentConditionFilter.
#'
#' Create a new gaSegmentConditionFilter object
#'
#' @param object An expression to be used as a non-sequential segment condition.
#' @param ... Other expressions to be ANDed to the first expression provided.
#' @param negation Logical TRUE or FALSE to match segments where this conditon
#'   has not been met.
#' @return a gaSegmentConditionFilter object.
#'
#' @export
setGeneric(
  "SegmentConditionFilter",
  function(object, ..., negation = FALSE) {},
  valueClass = "gaSegmentConditionFilter",
  useAsDefault = FALSE
)

#' Include.
#'
#' One or more segment conditions or sequences to include from the defined user
#' or session segment.
#'
#' @param object a condition or sequence to include
#' @param ... further conditions or sequences to include, i.e. ANDed.
#' @return a .gaSegmentFilter object with its negate slot set to FALSE.
#'
#' @export
setGeneric(
  "Include",
  function(object, ...) {},
  valueClass = c(".gaSegmentFilter", "gaSegmentFilterList"),
  useAsDefault = FALSE
)

#' Exclude.
#'
#' One or more segment conditions or sequences to exclude from the defined user
#' or session segment.
#'
#' @param object a condition or sequence to exclude
#' @param ... further conditions or sequences to exclude.
#' @return a .gaSegmentFilter object with its negate slot set to TRUE.
#'
#' @export
setGeneric(
  "Exclude",
  function(object, ...) {},
  valueClass = "gaSegmentFilterList",
  useAsDefault = TRUE
)

#' SegmentFilters.
#'
#' Create a new gaSegmentFilterList object
#'
#' A segment condition is either sequential or non-sequential. Sequential and
#' non-sequential conditoins can be combined using this function.
#'
#' @param object The first condition to be included in the segments definition.
#' @param ... Other conditions to be included in the segment definition.
#' @param scope The scope of this condition, either 'user' or 'session' level.
#' @return a gaSegmentFilterList object.
#'
#' @export
setGeneric(
  "SegmentFilters",
  function(object, ..., scope = "sessions") {},
  valueClass = "gaSegmentFilterList",
  useAsDefault = FALSE
)

#' IsNegated
#'
#' Tests whether a segment filter is negated.
#'
#' @param object an object to test for negation belonging to the superclass
#'   \code{.gaSegmentFilter}.
#'
#' @export
setGeneric(
  "IsNegated",
  function(object) {},
  valueClass = "logical",
  useAsDefault = FALSE
)

#' PerHit
#'
#' Set the scope of a gaMetExpr object to hit-level, or transforms a condition
#' filter to a sequence filter of length one (i.e. conditions to match a single
#' hit).
#'
#' @param object a gaMetExpr object to coerce to user-level.
#' @param ... Other conditions to be included in the segment definition.
#'
#' @export
setGeneric(
  "PerHit",
  function(object, ...){},
  valueClass = c("gaSegMetExpr", "gaSegmentSequenceFilter"),
  useAsDefault = FALSE
)

#' PerSession
#'
#' Set the scope of a gaSegmentFilterList or gaMetExpr object to session-level.
#'
#' @param object a gaSegmentFilterList or gaMetExpr object to coerce to session-level.
#' @param ... Other conditions to be included in the segment definition.
#'
#' @export
setGeneric(
  "PerSession",
  function(object, ...){},
  valueClass = c("gaSegmentFilterList", "gaSegMetExpr"),
  useAsDefault = FALSE
)

#' PerUser
#'
#' Set the scope of a gaSegmentFilterList or gaMetExpr object to user-level.
#'
#' @param object a gaSegmentFilterList or gaMetExpr object to coerce to user-level.
#' @param ... Other conditions to be included in the segment definition.
#'
#' @export
setGeneric(
  "PerUser",
  function(object, ...){},
  valueClass = c("gaSegmentFilterList", "gaSegMetExpr"),
  useAsDefault = FALSE
)

#' ScopeLevel.
#'
#' Get the scope level of a gaDynSegment or gaMetExpr
#'
#' @param object Segment condition or combined segment conditions or metric
#'   expression.
#' @param value If a new scope level is supplied, then this function will return
#'   an updated copy of the supplied object with the new scope applied.
#'
#' @export
#' @rdname ScopeLevel
setGeneric(
  "ScopeLevel",
  function(object, value) {},
  valueClass = "character",
  useAsDefault = FALSE
)

#' ScopeLevel<-.
#'
#' Set the scope level of a gaDynSegment or a gaMetExpr
#' For segments, one of 'users' or 'sessions'
#' For metric expressions one of 'perUser', 'perSession' or 'perHit'
#'
#' @export
#' @rdname ScopeLevel
setGeneric(
  "ScopeLevel<-",
  function(object, value) {
    object <- standardGeneric("ScopeLevel<-")
    validObject(object)
    object
  }
)

#' Segment.
#'
#' Get the segment.
#'
#' @param object An expression to coerce to a segment definition or segment ID
#' @param ... Other expressions to combine with the first expression, if
#'   appropriate.
#' @param scope The scope level to apply to the resulting segment definition.
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
#' @param value The segment definition or ID to set the segment parameter to.
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
#' @param object The object to be coerced to a TableFilter or the query object
#'   to apply a table filter to.
#' @param value The replacement table filter where \code{object} is a query.
#'
#' @export
#' @rdname TableFilter
setGeneric(
  "TableFilter",
  function(object, value) {},
  valueClass = c(".tableFilter", ".query"),
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
#' @param object The start date of the date range or a object to coerce to a
#'   date range. Alternatively, a query object to replace the date range of.
#' @param endDate The end date of the date range. Alternatively, if
#'   \code{object} is a '.query' object, then endDate is the replacement date
#'   range.
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
#' @param value The replacement date range.
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
  function(object, value) {},
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
  function(object, value) {},
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
#' @param object An object to coerce to a list of metrics, or a query object to
#'   replace the metrics of.
#' @param ... Further metrics to add to the resulting list or the replacement
#'   value for the metrics of the query object (if supplied).
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
#' @param value The replacement dimensions for the supplied object.
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
#' @param object An object to be coerced to a list of dimensions.
#' @param ... Other dimensions to add to the returned list, or if \code{object}
#'   is a query object, the replacement dimensions.
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
#' @param value The replacement dimensions for the supplied object.
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
#' @param object A character vector or list of dimensions or metrics to sort by.
#'   If character, then prefixing the dimension name with a "+" means ascending
#'   order or "-" for decending order. By default metrics are sorted in
#'   descending order, while dimensions are by default in ascending order.
#'   Alternatively, supply a query object and replacement dimensions and metrics
#'   or sort by.
#' @param ... further dimensions or metrics to sort by, or if \code{object} is a
#'   query then the replacement list of dimensions or metrics to sort by.
#' @param desc A logical vector, same length as the resulting list of dimension
#'   or metric variables, indicating which columns of the resulting query
#'   response should be sorted in decending order.
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
#' @param value The replacement dimensions and metrics for the supplied object.
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
#' @param object An object to coerce to a gaView class object or to get the
#'   gaView of, such as a query, default view of a web property, or the default
#'   view of the first web property in a Google Analytics account.
#' @param value The optional replacement view if the object supplied is a query,
#'   in which case GaView will return the modified query.
#'
#' @export
#' @rdname GaView
setGeneric(
  "GaView",
  function(object, value) {},
  valueClass = c("viewId", "gaView", ".query"),
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
#' @param object a query object.
#' @param value replacement value for the max-results parameter of the query.
#'
#' @export
#' @rdname MaxResults
setGeneric(
  "MaxResults",
  function(object, value) {},
  valueClass = "numeric",
  useAsDefault = FALSE
)

#' MaxResults<-.
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

#' GetGaData.
#'
#' Fetch the data for the Google Analytics API query.
#'
#' @param query The query execute and returned the processed response for.
#' @param creds The OAuth2.0 credentials to use for the request.
#' @param ... Other arguments to pass on to lower-level API functions.
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
#' @param object The object to get the credentials from.
#' @param value The replacement credentials for the supplied query object.
#' @param ... other arguments pass to \code{GoogleApiCreds}.
#'
#' @export
#' @rdname GaCreds
setGeneric(
  "GaCreds",
  function(object = "GANALYTICS", value = NULL, ...) {
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

