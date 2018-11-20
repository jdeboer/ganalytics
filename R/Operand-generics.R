#' Operand.
#'
#' Get the operand of an expression.
#'
#' @param object The object for which to set the operand of.
#' @param value Character or numeric. The value to set the operand to.
#'
#' @export
#' @rdname Operand
setGeneric(
  "Operand",
  function(object, value) {},
  valueClass = ".operand",
  useAsDefault = FALSE
)

#' Operand<-.
#'
#' Set the operand of an expression.
#'
#' @export
#' @rdname Operand
setGeneric(
  "Operand<-",
  function(object, value) {
    object <- standardGeneric("Operand<-")
    validObject(object)
    object
  }
)

