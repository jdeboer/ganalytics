#' Comparator.
#'
#' Get or create an comparator used in an expression.
#'
#' @param object The object to be coerced to a '.Comparator' class or to get the
#'   comparator from.
#' @param ... Used by certain methods.
#'
#' @export
#' @rdname Comparator
setGeneric(
  "Comparator",
  function(object, ...) {},
  valueClass = ".comparator",
  useAsDefault = FALSE
)

#' Comparator<-.
#'
#' Set the comparator used in an expression.
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
#' @param var dimension or metric object
#' @param operand operand object
#' @export
setGeneric(
  "%starts_with%",
  function(var, operand) {},
  useAsDefault = FALSE
)

#' @section \%ends_with\%:
#' A condition where the dimension (LHS) matches values that end with
#'   the character string given by the operand (RHS).
#' @rdname Comparator
#' @param var dimension or metric object
#' @param operand operand object
#' @export
setGeneric(
  "%ends_with%",
  function(var, operand) {},
  useAsDefault = FALSE
)

#' @section \%contains\%:
#' A condition where the dimension (LHS) matches values that contain
#'   the character string given by the operand (RHS).
#' @rdname Comparator
#' @param var dimension or metric object
#' @param operand operand object
#' @export
setGeneric(
  "%contains%",
  function(var, operand) {},
  useAsDefault = FALSE
)

#' @section \%matches\%:
#' A condition where the dimension (LHS) matches a regular
#'   expression given by the operand (RHS).
#' @rdname Comparator
#' @export
setGeneric(
  "%matches%",
  function(var, operand) {},
  useAsDefault = FALSE
)

#' @section \%between\%:
#' A condition where the dimension (LHS) is within the lower and
#'   uppoer bounds specified by first and second vector value (respectively) of
#'   the operand (RHS).
#' @rdname Comparator
#' @export
setGeneric(
  "%between%",
  function(var, operand) {},
  useAsDefault = FALSE
)

#' @section \%in\%:
#' A condition where the dimension (LHS) matches one of the
#'   values of a vecotr specified by the operand (RHS).
#' @rdname Comparator
#' @param x Dimension or metric object
#' @param table Operand object
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
  function(object) {},
  valueClass = "logical",
  useAsDefault = FALSE
)
