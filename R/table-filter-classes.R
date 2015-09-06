#' @include expression-classes.R
#' @include utils.R
#' @include all-generics.R
#' @importFrom methods setClass
NULL

# ---- table filter ----

#' `.tableFilter` class.
#'
#' An S4 class to represent a query table filter epxression.
#'
#' @rdname tableFilter-class
#' @keywords internal
#'
#' @export
setClass(
  ".tableFilter",
  contains = "andExpr",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to one class, i.e. either Metrics or Dimensions
    if (all(sapply(object@.Data, function(orExpr) {
      length(unique(sapply(orExpr, class))) == 1
    }))) {
      TRUE
    } else {
      return("An OR expression within a filter cannot mix metrics and dimensions.")
    }
    if (all(sapply(unlist(object@.Data), function(expr){
      !any(Comparator(expr) %in% c("[]", "<>"))
    }))) {
      TRUE
    } else {
      return("Filter expressions do not support '[]' or '<>' comparators.")
    }
  }
)

#' `gaFilter` class.
#'
#' An S4 class to represent a Core Reporting query table filter expression.
#'
#' @rdname gaFilter-class
#' @keywords internal
#'
#' @export
setClass(
  "gaFilter",
  contains = ".tableFilter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .gaExpr class
    if (all_inherit(unlist(object@.Data), ".gaExpr")) {
      TRUE
    } else {
      return("All expressions within a gaFilter must be of superclass .gaExpr")
    }
    if (all(sapply(unlist(object@.Data), GaVar) != "dateOfSession")) {
      TRUE
    } else {
      return("Filters do not support the 'dateOfSession' dimension. Use 'ga:date' instead.")
    }
    if (!any(sapply(unlist(object@.Data), GaVar) %in% c("<>", "[]"))) {
      TRUE
    } else {
      return("Filters do not support <> and [] comparators.")
    }
  }
)

#' `mcfFilter` class.
#'
#' An S4 class to represent Multi-Channel Funnel query table filter expression.
#'
#' @rdname mcfFilter-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfFilter",
  contains = ".tableFilter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .mcfExpr class
    if (all_inherit(unlist(object@.Data), ".mcfExpr")) {
      TRUE
    } else {
      return("All expressions within a mcfFilter must be of superclass .mcfExpr")
    }
  }
)

#' `rtFilter` class.
#'
#' An S4 class to represent Real-Time query table filter expression.
#'
#' @rdname rtFilter-class
#' @keywords internal
#'
#' @export
setClass(
  "rtFilter",
  contains = ".tableFilter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .mcfExpr class
    if (all_inherit(unlist(object@.Data), ".rtExpr")) {
      TRUE
    } else {
      return("All expressions within a rtFilter must be of superclass .rtExpr")
    }
  }
)
