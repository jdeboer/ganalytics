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
#' \donttest{
#'   myQuery <- GaQuery(view = 123456789)
#'   source_matches_google <- GaExpr("source", "~", "google")
#'   GaFilter(myQuery) <- source_matches_google
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
#' \donttest{
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
#' \donttest{
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
