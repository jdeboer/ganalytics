#' Comparator
#'
#' Get the comparator used in an expression or create a comparator object.
#'
#' @param object The object to be coerced to a '.comparator' subclass or the
#'   expression object of which to obtain its comparator.
#' @param ... Used by certain methods.
#'
#' @return An object belonging to the superclass \code{.comparator}
#'
#' @family comparator functions
#'
#' @export
#' @rdname Comparator
setGeneric(
  "Comparator",
  function(object, ...) {standardGeneric("Comparator")},
  valueClass = ".comparator"
)

#' Comparator<-
#'
#' Set the comparator of an expression.
#'
#' @param value The value to set the comparator to.
#'
#' @family comparator functions
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

#' Comparison operators
#'
#' Binary operators used to define Google Analytics filters and segments.
#'
#' @param var The name of a single Google Analytics dimension or metric, as a
#'   character string or a \code{.var} object generated with \code{\link{Var}}.
#' @param operand An operand appropriate to the selected \code{var} and
#'   comparison operator. A vector usually of length-one, or exactly length-two
#'   in the case of \code{\%between\%}, or at least length-two in the case of
#'   \code{\%in\%}. Usually either a character string or numeric value.
#'
#' @return an \code{expr} object.
#'
#' @family comparator functions
#'
#' @name comparators
NULL

#' @section \%starts_with\%:
#' A condition where the dimension (LHS) matches values that start with the
#' character string given by the operand (RHS).
#' @examples
#' Expr(~PagePath %starts_with% "/products")
#' @rdname comparators
#' @export
setGeneric(
  "%starts_with%",
  function(var, operand) {standardGeneric("%starts_with%")},
  valueClass = ".dimExpr"
)

# Base R functions: `startsWith` and `endsWith`

#' @section \%ends_with\%:
#' A condition where the dimension (LHS) matches values that end with the
#' character string given by the operand (RHS).
#' @examples
#' Expr(~PagePath %ends_with% "/index.html")
#' @rdname comparators
#' @export
setGeneric(
  "%ends_with%",
  function(var, operand) {standardGeneric("%ends_with%")},
  valueClass = ".dimExpr"
)

#' @section \%contains\%:
#' A condition where the dimension (LHS) matches values that contain the
#' character string given by the operand (RHS).
#' @examples
#' Expr(~PagePath %contains% "thank-you")
#' @rdname comparators
#' @export
setGeneric(
  "%contains%",
  function(var, operand) {standardGeneric("%contains%")},
  valueClass = ".dimExpr"
)

#' @section \%matches\%:
#' A condition where the dimension (LHS) matches a regular expression given by
#' the operand (RHS).
#' @examples
#' Expr(~PagePath %matches% "*.thank[\\-_]?you.*")
#' @rdname comparators
#' @export
setGeneric(
  "%matches%",
  function(var, operand) {standardGeneric("%matches%")},
  valueClass = ".dimExpr"
)

# Base R function `grepl`

#' @section \%between\%:
#' A condition where the var (LHS) is within the lower and upper bounds
#' specified by first and second vector value (respectively) of the operand
#' (RHS).
#' @examples
#' Expr(~transactionRevenue %between% c(200, 500))
#' @rdname comparators
#' @export
setGeneric(
  "%between%",
  function(var, operand) {standardGeneric("%between%")},
  valueClass = ".expr"
)

#' @section \%in\%:
#' A condition where the dimension (LHS) matches one of the values in the vector
#' specified by the operand (RHS).
#' @examples
#' Expr(~browser %in% c("Chrome", "Firefox"))
#' @rdname comparators
#' @export
setGeneric("%in%")

#' IsRegEx
#'
#' Checks for a regular expression.
#'
#' @param object An object to check if whether a regular expression.
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @export
setGeneric(
  "IsRegEx",
  function(object) {standardGeneric("IsRegEx")},
  valueClass = "logical"
)
