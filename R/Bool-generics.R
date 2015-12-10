
#' Not.
#'
#' Invert an expression, i.e. NOT.
#'
#' @param object An object to get the logical inverse of.
#'
#' @seealso \code{\link{And}} \code{\link{Or}} \code{\link{xor}}
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
#' @seealso \code{\link{And}} \code{\link{xor}} \code{\link{Not}}
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
#' @seealso \code{\link{Or}} \code{\link{xor}} \code{\link{Not}}
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
#' @seealso \code{\link{Or}} \code{\link{And}} \code{\link{Not}}
#'
#' @export
#' @rdname xor
setGeneric("xor")

