#' @include globaldata.R
#' @importFrom methods setClass setClassUnion setValidity
#' @importFrom assertthat validate_that
#' @importFrom stringr regex
NULL

# ---- expression operands ----

#' `.metOperand` class.
#'
#' An S4 class to represent the numerical value of a metric condition expression.
#'
#' @rdname metOperand-class
#' @keywords internal
#'
#' @export
setClass(
  ".metOperand",
  contains = "numeric"
)

#' `.dimOperand` class.
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

#' `.logicalDimOperand` class.
#'
#' An S4 class to represent the operand value of a logical dimension condition
#' expression.
#'
#' @rdname logicalDimOperand-class
#' @keywords internal
#'
#' @export
setClass(
  ".logicalDimOperand",
  contains = "logical"
)

setOldClass("regex")

#' `.regexDimOperand` class.
#'
#' An S4 class to represent the regex operand value of a dimension condition
#' expression.
#'
#' @rdname logicalDimOperand-class
#' @keywords internal
#'
#' @export
setClass(
  ".regexDimOperand",
  contains = ".dimOperand",
  slots = c(
    ignore_case = "logical"
  ),
  prototype = prototype(
    #stringr::regex(".*", ignore_case = TRUE),
    ignore_case = TRUE
  ),
  validity = function(object) {
    validate_that(
      length(object@case_sensitive) == 1L,
      assertthat::noNA(object@case_sensitive)
    )
  }
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
    if (length(object) == 2L) {
      if (object[1L] > object[2L]) {
        "The first value in a range must not be greater than the second"
      } else TRUE
    } else validate_that(length(object) <= 2L)
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
    validate_that(length(object) <= 10L)
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
  validate_that(length(object) == 1L)
})

setValidity(".rtOperand", function(object) {
  validate_that(length(object) == 1L)
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
  ".dimOperand", ".metOperand"
))

setValidity(".operand", function(object){
  validate_that(
    length(object) >= 1L,
    all(is.na(object) == FALSE)
  )
})
