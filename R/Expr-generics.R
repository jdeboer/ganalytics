#' Expr
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
  function(object, comparator, operand, metricScope = "") {
    standardGeneric("Expr")
  },
  valueClass = ".expr"
)

#' GaExpr
#'
#' Create a Core Reporting API expression.
#'
#' @examples
#' myQuery <- GaQuery(view = 123456789)
#' source_matches_google <- GaExpr("source", "~", "google")
#' TableFilter(myQuery) <- source_matches_google
#' @inheritParams Expr
#'
#' @family expression generators
#'
#' @export
setGeneric(
  "GaExpr",
  function(object, comparator, operand, metricScope = "") {
    standardGeneric("GaExpr")
  },
  valueClass = ".gaExpr"
)

#' McfExpr
#'
#' Create a Multi-Channel Funnel Reporting API expression.
#'
#' @examples
#' myQuery <- McfQuery(view = 123456789)
#' source_matches_google <- McfExpr("mcf:source", "~", "google")
#' TableFilter(myQuery) <- source_matches_google
#'
#' @inheritParams Expr
#'
#' @family expression generators
#'
#' @export
setGeneric(
  "McfExpr",
  function(object, comparator, operand) {standardGeneric("McfExpr")},
  valueClass = ".mcfExpr"
)

#' RtExpr
#'
#' Create a Real-Time Reporting API expression.
#'
#' @examples
#' myQuery <- RtQuery(view = 123456789)
#' source_matches_google <- RtExpr("rt:source", "~", "google")
#' TableFilter(myQuery) <- source_matches_google
#'
#' @inheritParams Expr
#'
#' @family expression generators
#'
#' @export
setGeneric(
  "RtExpr",
  function(object, comparator, operand) {standardGeneric("RtExpr")},
  valueClass = ".rtExpr"
)

#' ScopeLevel
#'
#' Get the scope level of a .gaSegmentFilter or gaMetExpr.
#'
#' @param object A .gaSegmentFilter or a metric expression.
#' @param value New scope level to return an updated copy of the object
#' with the new scope applied. For .gaSegmentFilters this can be
#' either 'users' or 'sessions'. For metric expressions use either 'perUser',
#' 'perSession', 'perHit' or 'perProduct'.
#' @return The scope level as a character string, a .gaSegmentFilter or gaMetExpr.
#'
#' @family dynamic segment functions
#'
#' @export
#' @rdname ScopeLevel
setGeneric(
  "ScopeLevel",
  function(object, value) {standardGeneric("ScopeLevel")},
  valueClass = c("character", ".gaSegmentFilter", "gaMetExpr")
)

#' ScopeLevel<-
#'
#' Set the scope level of a gaDynSegment or a gaMetExpr
#'
#' @family dynamic segment functions
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
