#' @importFrom methods setClass setClassUnion setValidity prototype
#' @importFrom stringr str_replace str_detect
#' @importFrom assertthat validate_that
#' @include ganalytics-package.R
#' @include utils.R
NULL

# Class definitions for ganalytics
# --------------------------------

# ---- dimension and metric variables ----

#' `.var` class.
#'
#' An S4 class to represent the name of a single Google Analytics dimension or
#' metric variable.
#'
#' @rdname var-class
#' @keywords internal
#'
#' @export
setClass(
  ".var",
  contains = "character",
  validity = function(object) {
    validate_that(length(object) == 1)
  }
)

#' `gaMetVar` class.
#'
#' An S4 class to represent a valid Google Analytics Core Reporting API metric
#' name.
#'
#' Set to "ga:sessions" by default.
#'
#' @rdname gaMetVar-class
#' @keywords internal
#'
#' @export
setClass(
  "gaMetVar",
  prototype = prototype("ga:sessions"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kGaVars$mets)) {
      TRUE
    } else {
      paste("Invalid GA metric name", object@.Data, sep = ": ")
    }
  }
)

#' `gaDimVar` class.
#'
#' An S4 class to represent a valid Google Analytics Core Reporting API dimension
#' name.
#'
#' Set to "ga:date" by default.
#'
#' @rdname gaDimVar-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDimVar",
  prototype = prototype("ga:date"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kGaVars$dims)) {
      TRUE
    } else {
      paste("Invalid GA dimension name", object@.Data, sep = ": ")
    }
  }
)

#' `mcfMetVar` class.
#'
#' An S4 class to represent a Multi-Channel Funnel metric.
#'
#' Set to "mcf:totalConversions" by default.
#'
#' @rdname mcfMetVar-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfMetVar",
  prototype = prototype("mcf:totalConversions"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kMcfVars$mets)) {
      TRUE
    } else {
      paste("Invalid MCF metric name", object@.Data, sep = ": ")
    }
  }
)

#' `mcfDimVar` class.
#'
#' An S4 class to represent a Multi-Channel Funnel dimension.
#'
#' Set to "mcf:nthDay" by default.
#'
#' @rdname mcfDimVar-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfDimVar",
  prototype = prototype("mcf:nthDay"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kMcfVars$dims)) {
      TRUE
    } else {
      paste("Invalid MCF dimension name", object@.Data, sep = ": ")
    }
  }
)

#' `rtMetVar` class.
#'
#' An S4 class to represent a Real-Time reporting metric.
#'
#' Set to "rt:pageviews" by default.
#'
#' @rdname rtMetVar-class
#' @keywords internal
#'
#' @export
setClass(
  "rtMetVar",
  prototype = prototype("rt:pageviews"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kRtVars$mets)) {
      TRUE
    } else {
      paste("Invalid RT metric name", object@.Data, sep = ": ")
    }
  }
)

#' `rtDimVar` class.
#'
#' An S4 class to represent a Real-Time reporting dimension.
#'
#' Set to "rt:minutesAgo" by default.
#'
#' @rdname rtDimVar-class
#' @keywords internal
#'
#' @export
setClass(
  "rtDimVar",
  prototype = prototype("rt:minutesAgo"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kRtVars$dims)) {
      TRUE
    } else {
      paste("Invalid RT dimension name", object@.Data, sep = ": ")
    }
  }
)

#' `.gaVar` class.
#'
#' An S4 class union to represent a Core Reporting variable.
#'
#' @docType class
#' @name .gaVar-class
#' @rdname gaVar-class
#' @keywords internal
#'
#' @exportClass .gaVar
setClassUnion(".gaVar", c("gaMetVar", "gaDimVar"))

#' `.mcfVar` class.
#'
#' An S4 class to represent a Multi-Channel Funnel variable.
#'
#' @docType class
#' @name .mcfVar-class
#' @rdname mcfVar-class
#' @keywords internal
#'
#' @exportClass .mcfVar
setClassUnion(".mcfVar", c("mcfMetVar", "mcfDimVar"))

#' `.rtVar` class.
#'
#' An S4 class to represent a Real-Time reporting variable.
#'
#' @docType class
#' @name .rtVar-class
#' @rdname rtVar-class
#' @keywords internal
#'
#' @exportClass .rtVar
setClassUnion(".rtVar", c("rtMetVar", "rtDimVar"))

#' `.metVar` class.
#'
#' An S4 class to represent a metric.
#'
#' @docType class
#' @name .metVar-class
#' @rdname metVar-class
#' @keywords internal
#'
#' @exportClass .metVar
setClassUnion(".metVar", c("gaMetVar", "mcfMetVar", "rtMetVar"))

#' `.dimVar` class.
#'
#' An S4 class to represent a dimension.
#'
#' @docType class
#' @name .dimVar-class
#' @rdname dimVar-class
#' @keywords internal
#'
#' @exportClass .dimVar
setClassUnion(".dimVar", c("gaDimVar", "mcfDimVar", "rtDimVar"))

# ---- expression comparators ----

#' `.comparator` class.
#'
#' An S4 class to represent a condition's comparator operator.
#'
#' Set to "==" (exact match) by default.
#'
#' @rdname comparator-class
#' @keywords internal
#'
#' @export
setClass(
  ".comparator",
  contains = "character",
  prototype = prototype("=="),
  validity = function(object) {
    validate_that(length(object) == 1)
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
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$met)
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
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$dim)
  }
)

#' `mcfMetComparator` class.
#'
#' An S4 class representing Multi-Channel Funnel metric condtion comparison
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

setClass(
  "mcfMetExpr",
  slots = c(
    var = "mcfMetVar",
    comparator = "mcfMetComparator",
    operand = "mcfMetOperand"
  ),
  contains = c(".mcfExpr", ".metExpr")
)

setClass(
  "mcfDimExpr",
  slots = c(
    var = "mcfDimVar",
    comparator = "mcfDimComparator",
    operand = "mcfDimOperand"
  ),
  contains = c(".mcfExpr", ".dimExpr")
)

setClass(
  "rtMetExpr",
  slots = c(
    var = "rtMetVar",
    comparator = "rtMetComparator",
    operand = "rtMetOperand"
  ),
  contains = c(".rtExpr", ".metExpr")
)

setClass(
  "rtDimExpr",
  slots = c(
    var = "rtDimVar",
    comparator = "rtDimComparator",
    operand = "rtDimOperand"
  ),
  contains = c(".rtExpr", ".dimExpr")
)

setClassUnion(".expr", c(
  "gaMetExpr", "gaDimExpr", "mcfMetExpr", "mcfDimExpr", "rtMetExpr", "rtDimExpr"
))

# ---- 'AND' and 'OR' compound expressions -------------------------------

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

# ---- table filter ----

setClass(
  ".tableFilter",
  contains = "andExpr",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to one class, i.e. either Metrics or Dimensions
    if (all(sapply(object@.Data, function(orExpr) {
      length(unique(sapply(orExpr, class))) == 1
    }))) {
      TRUE
    } else {
      return("An OR expression within a filter cannot mix metrics and dimensions.")
    }
    if (all(sapply(unlist(object@.Data), function(expr){
      !any(Comparator(expr) %in% c("[]", "<>"))
    }))) {
      TRUE
    } else {
      return("Filter expressions do not support '[]' or '<>' comparators.")
    }
  }
)

setClass(
  "gaFilter",
  contains = ".tableFilter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .gaExpr class
    if (all_inherit(unlist(object@.Data), ".gaExpr")) {
      TRUE
    } else {
      return("All expressions within a gaFilter must be of superclass .gaExpr")
    }
    if (all(sapply(unlist(object@.Data), GaVar) != "dateOfSession")) {
      TRUE
    } else {
      return("Filters do not support the 'dateOfSession' dimension. Use 'ga:date' instead.")
    }
    if (!any(sapply(unlist(object@.Data), GaVar) %in% c("<>", "[]"))) {
      TRUE
    } else {
      return("Filters do not support <> and [] comparators.")
    }
  }
)

setClass(
  "mcfFilter",
  contains = ".tableFilter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .mcfExpr class
    if (all_inherit(unlist(object@.Data), ".mcfExpr")) {
      TRUE
    } else {
      return("All expressions within a mcfFilter must be of superclass .mcfExpr")
    }
  }
)

setClass(
  "rtFilter",
  contains = ".tableFilter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .mcfExpr class
    if (all_inherit(unlist(object@.Data), ".rtExpr")) {
      TRUE
    } else {
      return("All expressions within a rtFilter must be of superclass .rtExpr")
    }
  }
)

# ---- dynamic and pre-defined segments ----

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

setClass(
  "gaSegmentConditionFilter",
  contains = c("gaSegmentCondition", ".gaSegmentFilter")
)

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

setClassUnion(".gaSegment", c("gaDynSegment", "gaSegmentId"))

setClass(
  "gaSegmentList",
  contains = "list",
  validity = function(object) {
    validate_that(all_inherit(object, ".gaSegment"))
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

# ---- dateRange ----

#' `dateRange` class.
#'
#' An S4 class to represent a date range.
#'
#' @rdname dateRange-class
#' @keywords internal
#'
#' @export
setClass(
  "dateRange",
  slots = c(
    startDate = "Date",
    endDate = "Date"
  ),
  prototype = prototype(
    startDate = Sys.Date() - 8,
    endDate = Sys.Date() - 1
  ),
  validity = function(object) {
    if (length(object@startDate) != length(object@endDate)) {
      "startDate and endDate must be the same length"
    } else if (all(object@startDate > object@endDate)) {
      "endDate cannot be before startDate"
    } else if (all(object@startDate < kGaDateOrigin)) {
      paste("Start date cannot preceed Google Analytics launch date:", kGaDateOrigin)
    } else TRUE
  }
)

# ---- query dimensions, metrics, and sortby lists ----

#' `.metrics` class.
#'
#' An S4 class to represent a list of metrics.
#'
#' @rdname metrics-class
#' @keywords internal
#'
#' @export
setClass(
  ".metrics",
  contains = "list",
  validity = function(object) {
    if (!all_inherit(object, ".metVar")) {
      return("Must be a list containing objects of class .metVar")
    } else TRUE
    if (length(object) > kGaMax$metrics) {
      return(paste("Maximum of", kGaMax$metrics, "metrics allowed."))
    } else TRUE
  }
)

#' `.dimensions` class.
#'
#' An S4 class to represent a list of dimensions.
#'
#' @rdname dimensions-class
#' @keywords internal
#'
#' @export
setClass(
  ".dimensions",
  contains = "list",
  validity = function(object) {
    if (!all_inherit(object, ".dimVar")) {
      return("Must be a list containing objects of class .dimVar")
    } else TRUE
    if (length(object) > kGaMax$dimensions) {
      return(paste("Maximum of", kGaMax$dimensions, "dimensions allowed."))
    } else TRUE
  }
)

#' `.sortBy` class.
#'
#' An S4 class to represent a list of variables to sort by.
#'
#' @rdname sortBy-class
#' @keywords internal
#'
#' @export
setClass(
  ".sortBy",
  slots = c(
    desc = "logical"
  ),
  prototype = prototype(
    list(),
    desc = logical()
  ),
  contains = "list",
  validity = function(object) {
    if (length(object@.Data) != length(object@desc)) {
      "List vector and desc vector must be of equal lengths"
    } else TRUE
  }
)

#' `.varList` class.
#'
#' An S4 class to represent .
#'
#' @docType class
#' @name .varList-class
#' @rdname varList-class
#' @keywords internal
#'
#' @exportClass .varList
setClassUnion(".varList", c(".metrics", ".dimensions", ".sortBy"))

setValidity(".varList", function(object) {
  if (!all_inherit(object, ".var") & length(object@.Data) > 0) {
    "Must be a list containing objects of class .var"
  } else TRUE
})

#' `gaMetrics` class.
#'
#' An S4 class to represent a list of Core Reporting metrics.
#'
#' @rdname gaMetrics-class
#' @keywords internal
#'
#' @export
setClass(
  "gaMetrics",
  prototype = prototype(
    list(new("gaMetVar"))
  ),
  contains = ".metrics"
)

#' `mcfMetrics` class.
#'
#' An S4 class to represent a list of Multi-channel funnel metrics.
#'
#' @rdname mcfMetrics-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfMetrics",
  prototype = prototype(
    list(new("mcfMetVar"))
  ),
  contains = ".metrics"
)

#' `rtMetrics` class.
#'
#' An S4 class to represent a list of Real-time metrics.
#'
#' @rdname rtMetrics-class
#' @keywords internal
#'
#' @export
setClass(
  "rtMetrics",
  prototype = prototype(
    list(new("rtMetVar"))
  ),
  contains = ".metrics"
)

#' `gaDimensions` class.
#'
#' An S4 class to represent a list of Core Reporting dimensions.
#'
#' @rdname gaDimensions-class
#' @keywords internal
#'
#' @export
setClass(
  "gaDimensions",
  prototype = prototype(
    list(new("gaDimVar"))
  ),
  contains = ".dimensions"
)

#' `mcfDimensions` class.
#'
#' An S4 class to represent a list of Multi-Channel Funnel dimensions.
#'
#' @rdname mcfDimensions-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfDimensions",
  prototype = prototype(
    list(new("mcfDimVar"))
  ),
  contains = ".dimensions"
)

#' `rtDimensions` class.
#'
#' An S4 class to represent a list of Real-Time reporting dimensions.
#'
#' @rdname rtDimensions-class
#' @keywords internal
#'
#' @export
setClass(
  "rtDimensions",
  prototype = prototype(
    list(new("rtDimVar"))
  ),
  contains = ".dimensions"
)

#' `gaSortBy` class.
#'
#' An S4 class to represent Core Reporting sorting lists.
#'
#' @rdname gaSortBy-class
#' @keywords internal
#'
#' @export
setClass("gaSortBy", contains = ".sortBy")

#' `mcfSortBy` class.
#'
#' An S4 class to represent Multi-Channel Funnel sorting lists.
#'
#' @rdname mcfSortBy-class
#' @keywords internal
#'
#' @export
setClass("mcfSortBy", contains = ".sortBy")

#' `rtSortBy` class.
#'
#' An S4 class to represent Real-Time variable sorting lists.
#'
#' @rdname rtSortBy-class
#' @keywords internal
#'
#' @export
setClass("rtSortBy", contains = ".sortBy")

#' `.gaVarList` class.
#'
#' An S4 class to represent Core Reporting variable lists.
#'
#' @docType class
#' @keywords internal
#' @name .gaVarList-class
#' @rdname gaVarList-class
#' @exportClass .gaVarList
setClassUnion(".gaVarList", c("gaMetrics", "gaDimensions", "gaSortBy"))

setValidity(".gaVarList", function(object) {
  if (!all_inherit(object, ".gaVar")) {
    "Must be a list containing objects of class .gaVar"
  } else TRUE
})

#' `mcfVarList` class.
#'
#' An S4 class to represent Multi-Channel Funnel variable lists.
#'
#' @docType class
#' @name .mcfVarList-class
#' @rdname mcfVarList-class
#' @keywords internal
#'
#' @exportClass .mcfVarList
setClassUnion(".mcfVarList", c("mcfMetrics", "mcfDimensions", "mcfSortBy"))

setValidity(".mcfVarList", function(object) {
  if (!all_inherit(object, ".mcfVar")) {
    "Must be a list containing objects of class .mcfVar"
  } else TRUE
})

#' `.rtVarList` class.
#'
#' An S4 class union of Real-Time variable lists.
#'
#' @docType class
#' @name .rtVarList-class
#' @rdname rtVarList-class
#' @keywords internal
#'
#' @exportClass .rtVarList
setClassUnion(".rtVarList", c("rtMetrics", "rtDimensions", "rtSortBy"))

setValidity(".rtVarList", function(object) {
  if (!all_inherit(object, ".rtVar")) {
    "Must be a list containing objects of class .rtVar"
  } else TRUE
})

# ---- View ID ----

#' `viewId` class.
#'
#' An S4 class to represent a Google Analytics view's ID.
#'
#' @rdname viewId-class
#' @keywords internal
#'
#' @export
setClass(
  "viewId",
  contains = "character",
  validity = function(object) {
    if (all(str_detect(object, "^ga:[0-9]+$"))) {
      TRUE
    } else {
      "viewId must be an string of digits preceeded by 'ga:'"
    }
  }
)

# -- GA query construct ----

#' `.query` class.
#'
#' An S4 class to represent a generalised reporting API query.
#'
#' @rdname query-class
#' @keywords internal
#'
#' @export
setClass(
  ".query",
  slots = c(
    viewId = "viewId",
    metrics = ".metrics",
    dimensions = ".dimensions",
    sortBy = ".sortBy",
    filters = ".tableFilter",
    maxResults = "numeric",
    creds = "list"
  ),
  prototype = prototype(
    maxResults = kGaMaxResults,
    creds = list()
  ),
  validity = function(object) {
    valid <- validate_that(
      length(object@maxResults) == 1,
      object@maxResults >= 1,
      length(object@metrics) >= 1
    )
    if (valid == TRUE) {
      if (object@maxResults > kGaMaxRows) {
        "maxResults cannot be greater than 1,000,000"
      } else if (!all(object@sortBy %in% union(object@metrics, object@dimensions))) {
        "sortBy must contain varNames also used as metrics and/or dimensions"
      } else TRUE
    } else valid
  }
)

#' `.standardQuery` class.
#'
#' An S4 class to represent a standard reporting API query.
#'
#' @rdname standardQuery-class
#' @keywords internal
#'
#' @export
setClass(
  ".standardQuery",
  slots = c(
    dateRange = "dateRange",
    samplingLevel = "character"
  ),
  prototype = prototype(
    dateRange = new("dateRange"),
    samplingLevel = "DEFAULT"
  ),
  contains = ".query",
  validity = function(object) {
    valid <- validate_that(length(object@samplingLevel) == 1)
    if (valid == TRUE) {
      if (!(object@samplingLevel %in% samplingLevel_levels)) {
        paste("samplingLevel must be one of:", paste(samplingLevel_levels, collapse = ", "))
      } else TRUE
    } else valid
  }
)

#' `gaQuery` class.
#'
#' An S4 class to represent a Core Reporting API query.
#'
#' @rdname gaQuery-class
#' @keywords internal
#'
#' @export
setClass(
  "gaQuery",
  slots = c(
    metrics = "gaMetrics",
    dimensions = "gaDimensions",
    sortBy = "gaSortBy",
    filters = "gaFilter",
    segments = ".gaSegment",
    buckets = "numeric"
  ),
  prototype = prototype(
    metrics = new("gaMetrics"),
    dimensions = new("gaDimensions"),
    sortBy = new("gaSortBy")
  ),
  contains = ".standardQuery"
)

#' `mcfQuery` class.
#'
#' An S4 class to represent a Multi-Channel Funnel Reporting API query.
#'
#' @rdname mcfQuery-class
#' @keywords internal
#'
#' @export
setClass(
  "mcfQuery",
  slots = c(
    metrics = "mcfMetrics",
    dimensions = "mcfDimensions",
    sortBy = "mcfSortBy",
    filters = "mcfFilter"
  ),
  prototype = prototype(
    metrics = new("mcfMetrics"),
    dimensions = new("mcfDimensions"),
    sortBy = new("mcfSortBy")
  ),
  contains = ".standardQuery"
)

#' `rtQuery` class.
#'
#' An S4 class to represent a Real-Time Reporting API query.
#'
#' @rdname rtQuery-class
#' @keywords internal
#'
#' @export
setClass(
  "rtQuery",
  slots = c(
    metrics = "rtMetrics",
    dimensions = "rtDimensions",
    sortBy = "rtSortBy",
    filters = "rtFilter"
  ),
  prototype = prototype(
    metrics = new("rtMetrics"),
    dimensions = new("rtDimensions"),
    sortBy = new("rtSortBy")
  ),
  contains = ".query"
)
