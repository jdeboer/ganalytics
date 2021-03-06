#' @include globaldata.R
#' @importFrom methods setClass setClassUnion prototype
#' @importFrom assertthat validate_that
NULL

# ---- expression comparators ----

#' `.comparator` class.
#'
#' An S4 class to represent a condition's comparator operator.
#'
#' Set to "==" (exact match) by default.
#'
#' @slot .Data A length-one character string for a valid comparator symbol.
#' @slot negated A length-one logical of either \code{TRUE} or \code{FALSE}
#'   indicating whether the comparator is a negated comparator such as in the
#'   case of 'not equal to' \code{!=}.
#'
#' @rdname comparator-class
#' @keywords internal
#'
#' @export
setClass(
  ".comparator",
  contains = "character",
  slots = c(
    negated = "logical"
  ),
  prototype = prototype(
    "==", negated = FALSE
  ),
  validity = function(object) {
    validate_that(
      length(object) == 1L,
      length(object@negated) == 1L,
      object@negated %in% c(FALSE, TRUE)
    )
  }
)

#' `gaMetComparator` class.
#'
#' An S4 class to represent a comparator operator for a Core Reporting metric
#' condition.
#'
#' @rdname gaMetComparator-class
#' @keywords internal
#'
#' @export
setClass(
  "gaMetComparator",
  contains = ".comparator",
  slots = c(
    operator = "character"
  ),
  prototype = prototype(
    operator = "EQUAL"
  ),
  validity = function(object) {
    validate_that(
      object@.Data %in% c(kGaOps$met, NA_character_),
      length(object@operator) == 1L,
      object@operator %in% names(kGa4Ops$metric_operators)
    )
  }
)

#' `gaDimComparator` class.
#'
#' An S4 class to represent a Core Reporting comparator operator for a dimension
#' condition.
#'
#' @rdname gaDimComparator-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDimComparator",
  contains = ".comparator",
  slots = c(
    operator = "character"
  ),
  prototype = prototype(
    operator = "EXACT"
  ),
  validity = function(object) {
    validate_that(
      object@.Data %in% c(kGaOps$dim, NA_character_),
      length(object@operator) == 1L,
      object@operator %in% names(kGa4Ops$dimension_operators)
    )
  }
)

#' `mcfMetComparator` class.
#'
#' An S4 class representing Multi-Channel Funnel metric condition comparison
#' operators.
#'
#' @rdname mcfMetComparator-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfMetComparator",
  contains = ".comparator",
  validity = function(object) {
    validate_that(object@.Data %in% kMcfOps$met)
  }
)

#' `mcfDimComparator` class.
#'
#' An S4 class representing Multi-Channel Funnel dimension conditional comparator
#' operators.
#'
#' @rdname mcfDimComparator-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfDimComparator",
  contains = ".comparator",
  validity = function(object) {
    validate_that(object@.Data %in% kMcfOps$dim)
  }
)

#' `rtMetComparator` class.
#'
#' An S4 class representing Real-Time metric condition comparator operators.
#'
#' @rdname rtMetComparator-class
#' @keywords internal
#'
#' @export
setClass(
  "rtMetComparator",
  contains = ".comparator",
  validity = function(object) {
    validate_that(object@.Data %in% kRtOps$met)
  }
)

#' `rtDimComparator` class.
#'
#' An S4 class to represent Real-Time dimension condition comparator operators.
#'
#' @rdname rtDimComparator-class
#' @keywords internal
#'
#' @export
setClass(
  "rtDimComparator",
  contains = ".comparator",
  validity = function(object) {
    validate_that(object@.Data %in% kRtOps$dim)
  }
)

#' `.gaComparator` class.
#'
#' An S4 class union represent conditional comparator operators.
#'
#' @docType class
#' @name .gaComparator-class
#' @rdname gaComparator-class
#' @keywords internal
#'
#' @exportClass .gaComparator
setClassUnion(".gaComparator" , c("gaMetComparator" , "gaDimComparator"))

#' `.mcfComparator` class.
#'
#' An S4 class union representing Multi-Channel Funnel condition comparator
#' operators.
#'
#' @docType class
#' @name .mcfComparator-class
#' @rdname mcfComparator-class
#' @keywords internal
#'
#' @exportClass .mcfComparator
setClassUnion(".mcfComparator", c("mcfMetComparator", "mcfDimComparator"))

#' `.rtComparator` class.
#'
#' An S4 class union representing Real-Time condition comparator operators.
#'
#' @docType class
#' @name .rtComparator-class
#' @rdname rtComparator-class
#' @keywords internal
#'
#' @exportClass .rtComparator
setClassUnion(".rtComparator" , c("rtMetComparator" , "rtDimComparator"))

#' `.dimComparator` class.
#'
#' An S4 class union representing dimension condition comparator operators.
#'
#' @docType class
#' @name .dimComparator-class
#' @rdname dimComparator-class
#' @keywords internal
#'
#' @exportClass .dimComparator
setClassUnion(".dimComparator", c("gaDimComparator", "mcfDimComparator", "rtDimComparator"))

#' `.metComparator` class.
#'
#' An S4 class union representing metric condition comparator operators.
#'
#' @docType class
#' @name .metComparator-class
#' @rdname metComparator-class
#' @keywords internal
#'
#' @exportClass .metComparator
setClassUnion(".metComparator", c("gaMetComparator", "mcfMetComparator", "rtMetComparator"))
