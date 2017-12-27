#' Expr.
#'
#' Define a Google Analytics expression.
#'
#' @param object A dimension or metric variable, or another object to be coerced
#'   to an .expr object.
#' @param comparator The comparator to use for the expression.
#' @param operand The operand to use for the expression.
#' @param metricScope Optional scope to use for segmentation if using a metric.
#'   Possible values include "perUser" or "perSession".
#'
#' @family expression generators
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
#' \dontrun{
#'   myQuery <- GaQuery(view = 123456789)
#'   source_matches_google <- GaExpr("source", "~", "google")
#'   TableFilter(myQuery) <- source_matches_google
#'   GetGaData(myQuery)
#' }
#' @inheritParams Expr
#'
#' @family expression generators
#'
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
#' \dontrun{
#'   myQuery <- McfQuery(view = 123456789)
#'   source_matches_google <- McfExpr("mcf:source", "~", "google")
#'   TableFilter(myQuery) <- source_matches_google
#'   GetData(myQuery)
#' }
#'
#' @inheritParams Expr
#'
#' @family expression generators
#'
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
#' \dontrun{
#'   myQuery <- RtQuery(view = 123456789)
#'   source_matches_google <- RtExpr("rt:source", "~", "google")
#'   TableFilter(myQuery) <- source_matches_google
#'   GetData(myQuery)
#' }
#'
#' @inheritParams Expr
#'
#' @family expression generators
#'
#' @export
setGeneric(
  "RtExpr",
  function(object, comparator, operand) {},
  valueClass = ".rtExpr",
  useAsDefault = FALSE
)

#' ScopeLevel.
#'
#' Get the scope level of a gaSegmentFilterList or gaMetExpr.
#'
#' @param object a gaSegmentFilterList or a metric expression.
#' @param value Optional new scope level to return an updated copy of the object
#' with the new scope applied.
#' @return the scope level as a character string, a gaSegmentFilterList or gaMetExpr.
#'
#' @export
#' @rdname ScopeLevel
setGeneric(
  "ScopeLevel",
  function(object, value) {},
  valueClass = c("character", "gaSegmentFilterList", "gaMetExpr"),
  useAsDefault = FALSE
)

#' ScopeLevel<-.
#'
#' Set the scope level of a gaDynSegment or a gaMetExpr
#'
#' @param object a gaSegmentFilterList or a metric expression.
#' @param value The scope level to apply. For gaSegmentFilterLists this can be
#' either 'users' or 'sessions'. For metric expressions use either 'perUser',
#' 'perSession', 'perHit' or 'perProduct'.
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
