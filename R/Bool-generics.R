#' Not
#'
#' \code{Not} inverts an expression, i.e. logical NOT.
#'
#' @param object An object to get the logical inverse of.
#'
#' @examples
#' source_matches_google <- Expr(~source %matches% "google")
#' source_not_matching_google <- Not(source_matches_google)
#' identical(source_not_matching_google, !source_matches_google)
#'
#' @family boolean functions
#'
#' @aliases `!`
#'
#' @export
setGeneric(
  "Not",
  function(object) {standardGeneric("Not")},
  valueClass = c(".comparator", ".compoundExpr", ".gaSegmentFilter")
)

#' Or
#'
#' Logical OR of two or more expressions.
#'
#' @param object An object to include within the ORed expression.
#' @param ... Additional objects to include within the ORed expression.
#'
#' @return An object of class \code{orExpr}.
#'
#' @note Google Analytics does not support ORing of ANDed expressions -- Only
#'   ANDing of ORed expresisons are supported. Consider De Morgan's laws for
#'   possible ways to work around this limitation.
#'
#' @examples
#' mobile_or_tablet <- Expr(~deviceCategory == "mobile") | Expr(~deviceCategory == "tablet")
#' converted <- Expr(~goalCompletionsAll > 0) | Expr(~transactions > 0)
#'
#' @family boolean functions
#'
#' @aliases `|`
#'
#' @export
setGeneric(
  "Or",
  function(object, ...) {standardGeneric("Or")},
  valueClass = "orExpr"
)

#' And
#'
#' Logical AND of two or more expressions.
#'
#' @param object An object to include within the ANDed expression.
#' @param ... Additional objects to include within the ANDed expression.
#'
#' @return An object of class \code{andExpr}
#'
#' @examples
#' purchased_on_mobile <- Expr(~deviceCategory == "mobile") & Expr(~transactions > 0)
#'
#' @family boolean functions
#'
#' @aliases `&`
#'
#' @export
setGeneric(
  "And",
  function(object, ...) {standardGeneric("And")},
  valueClass = "andExpr"
)

#' xor
#'
#' \code{xor} produces a compound expression that gives the EXCLUSIVE-OR of two expressions.
#'
#' @param x,y Conditions for an EXCLUSIVE-OR expression.
#'
#' @examples
#' either_enquired_or_downloaded <- xor(
#'   Expr(~eventCategory == "enquiry"),
#'   Expr(~eventCategory == "download")
#' )
#'
#' @family boolean functions
#'
#' @export
setGeneric("xor")

