#' @include globaldata.R
#' @importFrom methods setClass setClassUnion setValidity
#' @importFrom assertthat validate_that
NULL

# ---- expression operands ----

#' `metOperand` class.
#'
#' An S4 class to represent the numerical value of a metric condition expression.
#'
#' @rdname metOperand-class
#' @keywords internal
#'
#' @export
setClass(
  ".metOperand",
  contains = "numeric",
  validity = function(object) {
    validate_that(all(is.na(object) == FALSE))
  }
)

#' `dimOperand` class.
#'
#' An S4 class to represent the operand value of a dimension condition expression.
#'
#' @rdname dimOperand-class
#' @keywords internal
#'
#' @export
setClass(
  ".dimOperand",
  contains = "character"
)

#' `gaMetOperand` class.
#'
#' An S4 class to represent a Core-Reporting metric operand.
#'
#' @rdname gaMetOperand-class
#' @keywords internal
#'
#' @export
setClass(
  "gaMetOperand",
  contains = ".metOperand",
  validity = function(object) {
    if (length(object) == 2) {
      if (object[1] > object[2]) {
        "The first value in a range must not be greater than the second"
      } else TRUE
    } else validate_that(length(object) <= 2)
  }
)

#' `gaDimOperand` class.
#'
#' An S4 class to represent a Core-Reporting dimension operand.
#'
#' @rdname gaDimOperand-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDimOperand",
  contains = ".dimOperand",
  validity = function(object) {
    validate_that(length(object) <= 10)
  }
)

#' `mcfMetOperand` class.
#'
#' An S4 class to represent Multi-Channel metric condition operand.
#'
#' @rdname mcfMetOperand-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfMetOperand",
  contains = ".metOperand"
)

#' `mcfDimOperand` class.
#'
#' An S4 class to represent a Multi-Channel dimension condition operand.
#'
#' @rdname mcfDimOperand-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfDimOperand",
  contains = ".dimOperand"
)

#' `rtMetOperand` class.
#'
#' An S4 class to represent a Real-Time metric condition operand.
#'
#' @rdname rtMetOperand-class
#' @keywords internal
#'
#' @export
setClass(
  "rtMetOperand",
  contains = ".metOperand"
)

#' `rtDimOperand` class.
#'
#' An S4 class to represent a Real-Time dimension condition operand.
#'
#' @rdname rtDimOperand-class
#' @keywords internal
#'
#' @export
setClass(
  "rtDimOperand",
  contains = ".dimOperand"
)

#' `.gaOperand` class.
#'
#' An S4 class union representing Core-reporting condition operands.
#'
#' @docType class
#' @name .gaOperand-class
#' @rdname gaOperand-class
#' @keywords internal
#'
#' @exportClass .gaOperand
setClassUnion(".gaOperand", c("gaMetOperand", "gaDimOperand"))

#' `.mcfOperand` class.
#'
#' An S4 class union representing Multi-Channel Funnel condition operands.
#'
#' @docType class
#' @name .mcfOperand-class
#' @rdname mcfOperand-class
#' @keywords internal
#'
#' @exportClass .mcfOperand
setClassUnion(".mcfOperand", c("mcfMetOperand", "mcfDimOperand"))

#' `.rtOperand` class.
#'
#' An S4 class union representing Real-Time condition operands.
#'
#' @docType class
#' @name .rtOperand-class
#' @rdname rtOperand-class
#' @keywords internal
#'
#' @exportClass .rtOperand
setClassUnion(".rtOperand", c("rtMetOperand", "rtDimOperand"))

setValidity(".mcfOperand", function(object) {
  validate_that(length(object) == 1)
})

setValidity(".rtOperand", function(object) {
  validate_that(length(object) == 1)
})

#' `.operand` class.
#'
#' An S4 class union representing condition operands.
#'
#' @docType class
#' @name .operand-class
#' @rdname operand-class
#' @keywords internal
#'
#' @exportClass .operand
setClassUnion(".operand", c(
  "gaMetOperand", "gaDimOperand",
  "mcfMetOperand", "mcfDimOperand",
  "rtMetOperand", "rtDimOperand"
))

setValidity(".operand", function(object){
  validate_that(length(object) >= 1)
})
