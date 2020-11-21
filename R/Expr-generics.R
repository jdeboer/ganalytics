#' Expr
#'
#' Define a Google Analytics expression.
#'
#' @param object A dimension or metric variable, or another object to be coerced
#'   to an \code{.expr} object.
#' @param comparator The comparator to use for the expression.
#' @param operand The operand to use for the expression.
#' @param metricScope Optional scope to use in the case of metric variables for
#'   segmentation. Possible values include \code{"perUser"} or
#'   \code{"perSession"}.
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
#' @inheritParams Expr
#'
#' @examples
#' myQuery <- GaQuery(view = 123456789)
#' source_matches_google <- GaExpr("source", "~", "google")
#' TableFilter(myQuery) <- source_matches_google
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
#' @param object A dimension or metric variable, or another object to be coerced
#'   to an .expr object.
#' @param comparator The comparator to use for the expression.
#' @param operand The operand to use for the expression.
#'
#' @examples
#' myQuery <- McfQuery(view = 123456789)
#' source_matches_google <- McfExpr("mcf:source", "~", "google")
#' TableFilter(myQuery) <- source_matches_google
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
#' @inheritParams McfExpr
#'
#' @examples
#' myQuery <- RtQuery(view = 123456789)
#' source_matches_google <- RtExpr("rt:source", "~", "google")
#' TableFilter(myQuery) <- source_matches_google
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
#' Get or set the scope level of a \code{.gaSegmentFilter} or \code{gaMetExpr}.
#'
#' @param object A \code{.gaSegmentFilter} or \code{gaMetExpr} object.
#' @param value Optional new scope level to return an updated copy of the object
#'   with the new scope applied. For \code{.gaSegmentFilters} this can be either
#'   \code{'users'} or \code{'sessions'}. For metric expressions use either
#'   \code{'perUser'}, \code{'perSession'}, \code{'perHit'} or
#'   \code{'perProduct'}.
#' @return The scope level as a character string, or returns a
#'   \code{.gaSegmentFilter} or \code{gaMetExpr} object with the newly set
#'   scope.
#'
#' @examples
#' sessions_with_value <- Expr(~eventValue > 0, metricScope = "perSession")
#' ScopeLevel(sessions_with_value)
#' users_with_value_sessions <- Include(sessions_with_value)
#' ScopeLevel(users_with_value_sessions) <- "users"
#' sessions_with_value_segment <- ScopeLevel(users_with_value_sessions, "sessions")
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
#' Set the scope level of a \code{.gaSegmentFilter} or a \code{gaMetExpr}.
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
