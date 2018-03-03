#' @include expr-classes.R
#' @include Segment-generics.R
#' @include utils.R
#' @importFrom methods setClass setClassUnion prototype
#' @importFrom assertthat validate_that
NULL

# ---- dynamic and pre-defined segments ----

#' `gaSegmentCondition` class.
#'
#' An S4 class to represent an expression for a custom segment.
#'
#' @rdname gaSegmentCondition-class
#' @keywords internal
#'
#' @export
setClass(
  "gaSegmentCondition",
  contains = "andExpr",
  validity = function(object) {
    if (sum(rapply(object, is, class2 = ".gaExpr")) > 10L) {
      return("A maximum of 10 expressions per segment condition filter.")
    }
    if (!all(sapply(unlist(object@.Data), function(expr) {
      if (Comparator(expr) == "<>" & as.character(Var(expr)) == "dateOfSession") {
        (as.Date(Operand(expr))[2L] - as.Date(Operand(expr))[1L] + 1L) <= 31L
      } else TRUE
    }))) {
      return("The maximum date range for dateOfSession is 31 days.")
    }
    if (!all(sapply(unlist(object@.Data), function(expr) {
      if (as.character(Var(expr)) == "dateOfSession") {
        Comparator(expr) == "<>"
      } else TRUE
    }))) {
      return("The dateOfSession dimension can only be used with a <> comparator.")
    }
    TRUE
  }
)

#' `.gaSegmentFilter` class.
#'
#' An S4 superclass to represent a negatable filter expression for a custom
#' segment.
#'
#' @rdname gaSegmentFilter-class
#' @keywords internal
#'
#' @export
setClass(
  ".gaSegmentFilter",
  slots = c(
    negation = "logical",
    scope = "character"
  ),
  prototype = prototype(
    negation = FALSE,
    scope = "sessions"
  ),
  contains = "VIRTUAL",
  validity = function(object) {
    validate_that(
      length(object@negation) == 1L,
      object@negation %in% c(TRUE, FALSE),
      length(object@scope) == 1L,
      object@scope %in% c("users", "sessions")
    )
  }
)

#' `gaSegmentSequenceStep` class.
#'
#' An S4 class to represent a step within a sequence expression.
#'
#' @rdname gaSegmentSequenceStep-class
#' @keywords internal
#'
#' @export
setClass(
  "gaSegmentSequenceStep",
  slots = c(
    stepName = "character",
    immediatelyPrecedes = "logical"
  ),
  prototype = prototype(
    stepName = character(0),
    immediatelyPrecedes = FALSE
  ),
  contains = "gaSegmentCondition",
  validity = function(object) {
    validate_that(
      length(object@stepName) <= 1L,
      length(object@immediatelyPrecedes) == 1L,
      object@immediatelyPrecedes %in% c(TRUE, FALSE)
    )
  }
)

#' `gaSegmentSequenceFilter` class.
#'
#' An S4 class to represent a sequence expression as a segment filter.
#'
#' @rdname gaSegmentSequenceFilter-class
#' @keywords internal
#'
#' @export
setClass(
  "gaSegmentSequenceFilter",
  contains = c("list", ".gaSegmentFilter"),
  validity = function(object) {
    validate_that(length(object) <= 10L)
    if (!all_inherit(object@.Data, "gaSegmentSequenceStep")) {
      "All conditions within a sequence list must belong to the superclass 'gaSegmentSequenceStep'."
    } else {
      TRUE
    }
  }
)

#' `gaSegmentConditionFilter` class.
#'
#' An S4 class to represent a non-sequential condition-based segment filter
#' expression.
#'
#' @rdname gaSegmentConditionFilter-class
#' @keywords internal
#'
#' @export
setClass(
  "gaSegmentConditionFilter",
  contains = c("gaSegmentCondition", ".gaSegmentFilter")
)

#' `gaDynSegment` class.
#'
#' An S4 class to represent a list of segment filter expressions that belong to a
#' specified scope.
#'
#' @rdname gaDynSegment-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDynSegment",
  contains = "list",
  validity = function(object) {
    if (!all_inherit(object@.Data, ".gaSegmentFilter")) {
      "All conditions within a gaDynSegment list must belong to the superclass '.gaSegmentFilter'."
    } else if (sum(rapply(object, is, class2 = ".gaExpr")) > 10) {
      "A maximum of 10 dimension or metric conditions per segment."
    } else {
      TRUE
    }
  }
)

#' `gaSegmentId` class.
#'
#' An S4 class to represent a pre-defined custom or built-in segment by its ID.
#'
#' @rdname gaSegmentId-class
#' @keywords internal
#'
#' @export
setClass(
  "gaSegmentId",
  contains = "character",
  validity = function(object) {
    pattern <- "^gaid::\\-?[0-9A-Za-z]+$"
    if (length(object) != 1L) {
      "gaSegmentId must be a character vector of length 1"
    } else if (!grepl(pattern = pattern, x = object@.Data)) {
      paste("gaSegmentId must match the regular expression ", pattern, sep = "")
    } else {
      TRUE
    }
  }
)

#' `.gaSegment` class.
#'
#' An S4 class union representing dynamic and pre-defined segment expressions.
#'
#' @docType class
#' @name .gaSegment-class
#' @rdname gaSegment-class
#' @keywords internal
#'
#' @exportClass .gaSegment
setClassUnion(".gaSegment", c("gaDynSegment", "gaSegmentId"))

#' `gaSegmentList` class.
#'
#' An S4 class to represent a list of segment expressions to query.
#'
#' @rdname gaSegmentList-class
#' @keywords internal
#'
#' @export
setClass(
  "gaSegmentList",
  contains = "list",
  validity = function(object) {
    validate_that(all_inherit(object, ".gaSegment"))
  }
)
