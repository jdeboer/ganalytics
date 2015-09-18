#' @include var-classes.R
#' @include comparator-classes.R
#' @include operand-classes.R
#' @include all-generics.R
#' @include globaldata.R
#' @include utils.R
#' @importFrom methods setClass setClassUnion
#' @importFrom assertthat validate_that
NULL

# ---- simple expressions -------------------------------------------------------

#' `gaExpr` class.
#'
#' An S4 class to represent a Core Reporting API condition expression.
#'
#' @rdname gaExpr-class
#' @keywords internal
#'
#' @export
setClass(
  ".gaExpr",
  slots = c(
    var = ".gaVar",
    comparator = ".gaComparator",
    operand = ".gaOperand"
  )
)

#' `mcfExpr` class.
#'
#' An S4 class to represent a Multi-Channel funnel condition expression.
#'
#' @rdname mcfExpr-class
#' @keywords internal
#'
#' @export
setClass(
  ".mcfExpr",
  slots = c(
    var = ".mcfVar",
    comparator = ".mcfComparator",
    operand = ".mcfOperand"
  )
)

#' `rtExpr` class.
#'
#' An S4 class to represent a Real-Time API condition expression.
#'
#' @rdname rtExpr-class
#' @keywords internal
#'
#' @export
setClass(
  ".rtExpr",
  slots = c(
    var = ".rtVar",
    comparator = ".rtComparator",
    operand = ".rtOperand"
  )
)

#' `.metExpr` class.
#'
#' An S4 class to represent a metric condition expression.
#'
#' @rdname metExpr-class
#' @keywords internal
#'
#' @export
setClass(
  ".metExpr",
  slots = c(
    var = ".metVar",
    comparator = ".metComparator",
    operand = ".metOperand"
  )
)

#' `.dimExpr` class.
#'
#' An S4 class to represent a dimension condition expression.
#'
#' @rdname dimExpr-class
#' @keywords internal
#'
#' @export
setClass(
  ".dimExpr",
  slots = c(
    var = ".dimVar",
    comparator = ".dimComparator",
    operand = ".dimOperand"
  )
)

#' `gaMetExpr` class.
#'
#' An S4 class to represent a Core-Reporting metric condition expression.
#'
#' @rdname gaMetExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "gaMetExpr",
  slots = c(
    var = "gaMetVar",
    comparator = "gaMetComparator",
    operand = "gaMetOperand"
  ),
  contains = c(".gaExpr", ".metExpr"),
  validity = function(object) {
    if (object@comparator == "<>") {
      if (length(object@operand) != 2) {
        "operand must be of length 2 when using a range comparator '<>'."
      } else TRUE
    } else {
      if (length(object@operand) != 1) {
        "operand must be of length 1 unless using a range comparator '<>'."
      } else TRUE
    }
  }
)

#' `gaSegMetExpr` class.
#'
#' An S4 class to represent a Core-Reporting metric condition for use in segment
#' expressions.
#'
#' @rdname gaSegMetExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "gaSegMetExpr",
  slots = c(
    metricScope = "character"
  ),
  prototype = prototype(
    metricScope = "perSession"
  ),
  contains = "gaMetExpr",
  validity = function(object) {
    validate_that(
      length(object@metricScope) == 1,
      object@metricScope %in% c("perUser", "perSession", "perHit")
    )
  }
)

#' `gaDimExpr` class.
#'
#' An S4 class to represent a Core-Reporting dimension condition expression.
#'
#' @rdname gaDimExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDimExpr",
  slots = c(
    var = "gaDimVar",
    comparator = "gaDimComparator",
    operand = "gaDimOperand"
  ),
  contains = c(".gaExpr", ".dimExpr"),
  validity = function(object) {
    if (object@comparator == "<>") {
      rangeDimVars <- unlist(kGaDimTypes[c("nums", "dates", "orderedIntFactors")], use.names = FALSE)
      if (!(object@var %in% rangeDimVars)) {
        return("A range comparator only supports numerical dimensions or metrics")
      }
    }
    if (!(length(object@operand) == 1 | object@comparator %in% c("<>", "[]"))) {
      return("operand must be of length 1 unless using a range '<>' or list '[]' comparator.")
    } else if (!(length(object@operand) <= 2 | object@comparator == "[]")) {
      return("operand may only be greater than length 2 if using a list comparator '[]'.")
    } else if (IsRegEx(object@comparator)) {
      if (nchar(object@operand) > 128) {
        return(paste0("Regular expressions in GA Dimension Expressions cannot exceed 128 chars. Length = ", nchar(object@operand)))
      }
    }
    if (object@comparator %in% c("!=", "==", "<>", "[]")) {
      ValidGaOperand(object@var, object@operand)
    } else TRUE
  }
)

#' `mcfMetExpr` class.
#'
#' An S4 class to represent a Multi-Channel Funnel metric condition expression.
#'
#' @rdname mcfMetExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfMetExpr",
  slots = c(
    var = "mcfMetVar",
    comparator = "mcfMetComparator",
    operand = "mcfMetOperand"
  ),
  contains = c(".mcfExpr", ".metExpr")
)

#' `mcfDimExpr` class.
#'
#' An S4 class to represent a Multi-Channel Funnel dimension condition
#' expression.
#'
#' @rdname mcfDimExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfDimExpr",
  slots = c(
    var = "mcfDimVar",
    comparator = "mcfDimComparator",
    operand = "mcfDimOperand"
  ),
  contains = c(".mcfExpr", ".dimExpr")
)

#' `rtMetExpr` class.
#'
#' An S4 class to represent a Real-Time metric condition expression.
#'
#' @rdname rtMetExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "rtMetExpr",
  slots = c(
    var = "rtMetVar",
    comparator = "rtMetComparator",
    operand = "rtMetOperand"
  ),
  contains = c(".rtExpr", ".metExpr")
)

#' `rtDimExpr` class.
#'
#' An S4 class to represent Real-Time dimension condition expression.
#'
#' @rdname rtDimExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "rtDimExpr",
  slots = c(
    var = "rtDimVar",
    comparator = "rtDimComparator",
    operand = "rtDimOperand"
  ),
  contains = c(".rtExpr", ".dimExpr")
)

#' `.expr` class.
#'
#' An S4 class union representing condition expressions.
#'
#' @docType class
#' @name .expr-class
#' @rdname expr-class
#' @keywords internal
#'
#' @exportClass .expr
setClassUnion(".expr", c(
  "gaMetExpr", "gaDimExpr", "mcfMetExpr", "mcfDimExpr", "rtMetExpr", "rtDimExpr"
))

# ---- 'AND' and 'OR' compound expressions -------------------------------

#' `orExpr` class.
#'
#' An S4 class to represent an expression of ORed conditions.
#'
#' @rdname orExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "orExpr",
  contains = "list",
  validity = function(object) {
    if (all_inherit(object@.Data, ".expr")) {
      TRUE
    } else {
      "An orExpr must be a list containing objects that all inherit from the class .expr"
    }
  }
)

#' `andExpr` class.
#'
#' An S4 class to represent an expression of ANDed 'OR' expression.
#'
#' @rdname andExpr-class
#' @keywords internal
#'
#' @export
setClass(
  "andExpr",
  contains = "list",
  validity = function(object) {
    if (all_inherit(object@.Data, "orExpr")) {
      TRUE
    } else {
      "An andExpr must be a list containing objects all of the class orExpr"
    }
  }
)

# ---- Simple and compound expression class union ----

#' `.compoundExpr` class.
#'
#' An S4 class representing an expression containing one or more conditions.
#'
#' @docType class
#' @name .compoundExpr-class
#' @rdname compoundExpr-class
#' @keywords internal
#'
#' @exportClass .compoundExpr
setClassUnion(".compoundExpr", c(
  ".expr", "orExpr", "andExpr",
  "gaMetExpr", "gaDimExpr", "mcfMetExpr", "mcfDimExpr", "rtMetExpr", "rtDimExpr"
))
