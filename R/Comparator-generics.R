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
  valueClass = ".comparator",
  useAsDefault = FALSE
  function(object, ...) {standardGeneric("Comparator")},
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
  valueClass = ".dimExpr",
  useAsDefault = FALSE
  function(var, operand) {standardGeneric("%starts_with%")},
)

#' @section \%ends_with\%:
#' A condition where the dimension (LHS) matches values that end with
#'   the character string given by the operand (RHS).
#' @rdname Comparator
#' @family comparators
#' @export
setGeneric(
  "%ends_with%",
  valueClass = ".dimExpr",
  useAsDefault = FALSE
  function(var, operand) {standardGeneric("%ends_with%")},
)

#' @section \%contains\%:
#' A condition where the dimension (LHS) matches values that contain
#'   the character string given by the operand (RHS).
#' @rdname Comparator
#' @family comparators
#' @export
setGeneric(
  "%contains%",
  valueClass = ".dimExpr",
  useAsDefault = FALSE
  function(var, operand) {standardGeneric("%contains%")},
)

#' @section \%matches\%:
#' A condition where the dimension (LHS) matches a regular
#'   expression given by the operand (RHS).
#' @rdname Comparator
#' @family comparators
#' @export
setGeneric(
  "%matches%",
  valueClass = ".dimExpr",
  useAsDefault = FALSE
  function(var, operand) {standardGeneric("%matches%")},
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
  valueClass = ".expr",
  useAsDefault = FALSE
  function(var, operand) {standardGeneric("%between%")},
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
  valueClass = "logical",
  useAsDefault = FALSE
  function(object) {standardGeneric("IsRegEx")},
)
