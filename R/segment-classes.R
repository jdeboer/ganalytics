#' @include expression-classes.R
#' @include all-generics.R
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
    if (all(sapply(unlist(object@.Data), function(expr) {
      if (Comparator(expr) == "<>" & Var(expr) == "dateOfSession") {
        (Operand(expr)[2] - Operand(expr)[1] + 1) <= 31
      } else TRUE
    }))) {
      TRUE
    } else {
      return("The maximum date range for dateOfSession is 31 days.")
    }
    if (all(sapply(unlist(object@.Data), function(expr) {
      if (Var(expr) == "dateOfSession") {
        Comparator(expr) == "<>"
      } else TRUE
    }))) {
      TRUE
    } else {
      return("The dateOfSession dimension can only be used with a <> comparator.")
    }
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
    negation = "logical"
  ),
  prototype = prototype(
    negation = FALSE
  ),
  contains = "VIRTUAL",
  validity = function(object) {
    validate_that(
      length(object@negation) == 1
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
    immediatelyPrecedes = "logical"
  ),
  prototype = prototype(
    immediatelyPrecedes = FALSE
  ),
  contains = "gaSegmentCondition",
  validity = function(object) {
    validate_that(
      length(object@immediatelyPrecedes) == 1
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
    if (all_inherit(object@.Data, "gaSegmentSequenceStep")) {
      TRUE
    } else {
      "All conditions within a sequence list must belong to the superclass 'gaSegmentSequenceStep'."
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

#' `gaSegmentFilterList` class.
#'
#' An S4 class to represent a list of segment filter expressions that belong to a
#' specified scope.
#'
#' @rdname gaSegmentFilterList-class
#' @keywords internal
#'
#' @export
setClass(
  "gaSegmentFilterList",
  slots = c(
    conditionScope = "character"
  ),
  prototype = prototype(
    conditionScope = "sessions"
  ),
  contains = "list",
  validity = function(object) {
    if (!all_inherit(object@.Data, ".gaSegmentFilter")) {
      "All conditions within a gaSegmentFilterList list must belong to the superclass '.gaSegmentFilter'."
    } else if (length(object@conditionScope) != 1) {
      "Slot 'conditionScope' must be of length 1."
    } else if (!(object@conditionScope %in% c("users", "sessions"))) {
      "Slot 'conditionScope' must be either 'users' or 'sessions'."
    } else TRUE
  }
)

#' `gaDynSegment` class.
#'
#' An S4 class to represent a list of scoped segment filter lists.
#'
#' @rdname gaDynSegment-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDynSegment",
  contains = "list",
  validity = function(object) {
    if (!all_inherit(object@.Data, "gaSegmentFilterList")) {
      "All objects with a gaDynSegment list must belong to the class 'gaSegmentFilterList'."
    } else if (identical(nchar(as(object, "character")) > 1024, TRUE)) {
      "The maximum expression length for dimension conditions is 1024 characters."
    } else if (sum(rapply(object, is, class2 = ".gaExpr")) > 10) {
      "A maximum of 10 dimension or metric conditions per segment."
    } else TRUE
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
    if (length(object) != 1) {
      "gaSegmentId must be a character vector of length 1"
    } else if (!grepl(pattern = pattern, x = object@.Data)) {
      paste("gaSegmentId must match the regular expression ", pattern, sep = "")
    } else TRUE
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
