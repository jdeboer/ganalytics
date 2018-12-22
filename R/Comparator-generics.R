#' Comparator.
#'
#' Get a comparator used in an expression or create a comparator object.
#'
#' @param object The object to be coerced to a '.Comparator' subclass or the
#'   expression object of which to obtain its comparator.
#' @param ... Used by certain methods.
#'
#' @export
#' @rdname Comparator
setGeneric(
  "Comparator",
  function(object, ...) {standardGeneric("Comparator")},
  valueClass = ".comparator"
)

#' Comparator<-.
#'
#' Set the comparator of an expression.
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

#' @section \%starts_with\%:
#' A condition where the dimension (LHS) matches values that start with
#'   the character string given by the operand (RHS).
#' @rdname Comparator
#' @param var dimension object
#' @param operand operand object
#' @family comparators
#' @export
setGeneric(
  "%starts_with%",
  function(var, operand) {standardGeneric("%starts_with%")},
  valueClass = ".dimExpr"
)

#' @section \%ends_with\%:
#' A condition where the dimension (LHS) matches values that end with
#'   the character string given by the operand (RHS).
#' @rdname Comparator
#' @family comparators
#' @export
setGeneric(
  "%ends_with%",
  function(var, operand) {standardGeneric("%ends_with%")},
  valueClass = ".dimExpr"
)

#' @section \%contains\%:
#' A condition where the dimension (LHS) matches values that contain
#'   the character string given by the operand (RHS).
#' @rdname Comparator
#' @family comparators
#' @export
setGeneric(
  "%contains%",
  function(var, operand) {standardGeneric("%contains%")},
  valueClass = ".dimExpr"
)

#' @section \%matches\%:
#' A condition where the dimension (LHS) matches a regular
#'   expression given by the operand (RHS).
#' @rdname Comparator
#' @family comparators
#' @export
setGeneric(
  "%matches%",
  function(var, operand) {standardGeneric("%matches%")},
  valueClass = ".dimExpr"
)

#' @section \%between\%:
#' A condition where the var (LHS) is within the lower and
#'   upper bounds specified by first and second vector value (respectively) of
#'   the operand (RHS).
#' @rdname Comparator
#' @family comparators
#' @export
setGeneric(
  "%between%",
  function(var, operand) {standardGeneric("%between%")},
  valueClass = ".expr"
)

#' @section \%in\%:
#' A condition where the dimension (LHS) matches one of the
#'   values in the vector specified by the operand (RHS).
#' @rdname Comparator
#' @param x Dimension or metric object
#' @param table Operand object
#' @family comparators
#' @export
setGeneric("%in%")

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
  function(object) {standardGeneric("IsRegEx")},
  valueClass = "logical"
)
