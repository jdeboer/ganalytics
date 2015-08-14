#' @importFrom methods setClass setClassUnion setValidity prototype
#' @importFrom stringr str_replace str_detect
#' @importFrom assertthat validate_that
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# Class definitions for ganalytics
# --------------------------------

# ---- dimension and metric variables ----

setClass(
  ".var",
  contains = "character",
  validity = function(object) {
    validate_that(length(object) == 1)
  }
)

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

setClass(
  "rtMetVar",
  prototype = prototype("rt:activeUsers"),
  contains = ".var",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kRtVars$mets)) {
      TRUE
    } else {
      paste("Invalid RT metric name", object@.Data, sep = ": ")
    }
  }
)

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

setClassUnion(".gaVar", c("gaMetVar", "gaDimVar"))
setClassUnion(".mcfVar", c("mcfMetVar", "mcfDimVar"))
setClassUnion(".rtVar", c("rtMetVar", "rtDimVar"))

setClassUnion(".metVar", c("gaMetVar", "mcfMetVar", "rtMetVar"))
setClassUnion(".dimVar", c("gaDimVar", "mcfDimVar", "rtDimVar"))

# ---- expression operators ----

setClass(
  ".operator",
  contains = "character",
  prototype = prototype("=="),
  validity = function(object) {
    validate_that(length(object) == 1)
  }
)

setClass(
  "gaMetOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$met)
  }
)

setClass(
  "gaDimOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$dim)
  }
)

setClass(
  "mcfMetOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kMcfOps$met)
  }
)

setClass(
  "mcfDimOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kMcfOps$dim)
  }
)

setClass(
  "rtMetOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kRtOps$met)
  }
)

setClass(
  "rtDimOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kRtOps$dim)
  }
)

setClassUnion(".gaOperator" , c("gaMetOperator" , "gaDimOperator"))
setClassUnion(".mcfOperator", c("mcfMetOperator", "mcfDimOperator"))
setClassUnion(".rtOperator" , c("rtMetOperator" , "rtDimOperator"))

setClassUnion(".dimOperator", c("gaDimOperator", "mcfDimOperator", "rtDimOperator"))
setClassUnion(".metOperator", c("gaMetOperator", "mcfMetOperator", "rtMetOperator"))

# ---- expression operands ----

setClass(
  ".metOperand",
  contains = "numeric",
  validity = function(object) {
    validate_that(all(is.na(object) == FALSE))
  }
)

setClass(
  ".dimOperand",
  contains = "character"
)

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

setClass(
  "gaDimOperand",
  contains = ".dimOperand",
  validity = function(object) {
    validate_that(length(object) <= 10)
  }
)

setClass(
  "mcfMetOperand",
  contains = ".metOperand"
)

setClass(
  "mcfDimOperand",
  contains = ".dimOperand"
)

setClass(
  "rtMetOperand",
  contains = ".metOperand"
)

setClass(
  "rtDimOperand",
  contains = ".dimOperand"
)

setClassUnion(".gaOperand", c("gaMetOperand", "gaDimOperand"))
setClassUnion(".mcfOperand", c("mcfMetOperand", "mcfDimOperand"))
setClassUnion(".rtOperand", c("rtMetOperand", "rtDimOperand"))

setValidity(".mcfOperand", function(object) {
  validate_that(length(object) == 1)
})

setValidity(".rtOperand", function(object) {
  validate_that(length(object) == 1)
})

setClassUnion(".operand", c(
  "gaMetOperand", "gaDimOperand",
  "mcfMetOperand", "mcfDimOperand",
  "rtMetOperand", "rtDimOperand"
))

setValidity(".operand", function(object){
  validate_that(length(object) >= 1)
})

# ---- simple expressions -------------------------------------------------------

setClass(
  ".gaExpr",
  slots = c(
    var = ".gaVar",
    operator = ".gaOperator",
    operand = ".gaOperand"
  )
)

setClass(
  ".mcfExpr",
  slots = c(
    var = ".mcfVar",
    operator = ".mcfOperator",
    operand = ".mcfOperand"
  )
)

setClass(
  ".rtExpr",
  slots = c(
    var = ".rtVar",
    operator = ".rtOperator",
    operand = ".rtOperand"
  )
)

setClass(
  ".metExpr",
  slots = c(
    var = ".metVar",
    operator = ".metOperator",
    operand = ".metOperand"
  )
)

setClass(
  ".dimExpr",
  slots = c(
    var = ".dimVar",
    operator = ".dimOperator",
    operand = ".dimOperand"
  )
)

setClass(
  "gaMetExpr",
  slots = c(
    var = "gaMetVar",
    operator = "gaMetOperator",
    operand = "gaMetOperand"
  ),
  contains = c(".gaExpr", ".metExpr"),
  validity = function(object) {
    if (object@operator == "<>") {
      if (length(object@operand) != 2) {
        "operand must be of length 2 when using a range operator '<>'."
      } else TRUE
    } else {
      if (length(object@operand) != 1) {
        "operand must be of length 1 unless using a range operator '<>'."
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
    operator = "gaDimOperator",
    operand = "gaDimOperand"
  ),
  contains = c(".gaExpr", ".dimExpr"),
  validity = function(object) {
    if (object@operator == "<>") {
      rangeDimVars <- unlist(kGaDimTypes[c("nums", "dates", "orderedIntFactors")], use.names = FALSE)
      if (!(object@var %in% rangeDimVars)) {
        return("A range operator only supports numerical dimensions or metrics")
      }
    }
    if (!(length(object@operand) == 1 | object@operator %in% c("<>", "[]"))) {
      return("operand must be of length 1 unless using a range '<>' or list '[]' operator.")
    } else if (!(length(object@operand) <= 2 | object@operator == "[]")) {
      return("operand may only be greater than length 2 if using a list operator '[]'.")
    } else if (IsRegEx(object@operator)) {
      if (nchar(object@operand) > 128) {
        return(paste0("Regular expressions in GA Dimension Expressions cannot exceed 128 chars. Length = ", nchar(object@operand)))
      }
    }
    if (object@operator %in% c("!=", "==", "<>", "[]")) {
      ValidGaOperand(object@var, object@operand)
    } else TRUE
  }
)

setClass(
  "mcfMetExpr",
  slots = c(
    var = "mcfMetVar",
    operator = "mcfMetOperator",
    operand = "mcfMetOperand"
  ),
  contains = c(".mcfExpr", ".metExpr")
)

setClass(
  "mcfDimExpr",
  slots = c(
    var = "mcfDimVar",
    operator = "mcfDimOperator",
    operand = "mcfDimOperand"
  ),
  contains = c(".mcfExpr", ".dimExpr")
)

setClass(
  "rtMetExpr",
  slots = c(
    var = "rtMetVar",
    operator = "rtMetOperator",
    operand = "rtMetOperand"
  ),
  contains = c(".rtExpr", ".metExpr")
)

setClass(
  "rtDimExpr",
  slots = c(
    var = "rtDimVar",
    operator = "rtDimOperator",
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
      !any(Operator(expr) %in% c("[]", "<>"))
    }))) {
      TRUE
    } else {
      return("Filter expressions do not support '[]' or '<>' operators.")
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
      return("Filters do not support <> and [] operators.")
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

## TO DO - rename these classes to be clear they are segmentation classes.
## or consider how these classes can extend to MCF paths, which are a form of sequences.


setClass(
  "gaDimensionOrMetricCondition",
  contains = "andExpr",
  validity = function(object) {
    if (all(sapply(unlist(object@.Data), function(expr) {
      if (Operator(expr) == "<>" & Var(expr) == "dateOfSession") {
        (Operand(expr)[2] - Operand(expr)[1] + 1) <= 31
      } else TRUE
    }))) {
      TRUE
    } else {
      return("The maximum date range for dateOfSession is 31 days.")
    }
    if (all(sapply(unlist(object@.Data), function(expr) {
      if (Var(expr) == "dateOfSession") {
        Operator(expr) == "<>"
      } else TRUE
    }))) {
      TRUE
    } else {
      return("The dateOfSession dimension can only be used with a <> operator.")
    }
  }
)

setClass(
  ".gaSimpleOrSequence",
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
  "gaSequenceStep",
  slots = c(
    immediatelyPrecedes = "logical"
  ),
  prototype = prototype(
    immediatelyPrecedes = FALSE
  ),
  contains = "gaDimensionOrMetricCondition",
  validity = function(object) {
    validate_that(
      length(object@immediatelyPrecedes) == 1
    )
  }
)

setClass(
  "gaSequenceCondition",
  contains = c("list", ".gaSimpleOrSequence"),
  validity = function(object) {
    if (all_inherit(object@.Data, "gaSequenceStep")) {
      TRUE
    } else {
      "All conditions within a sequence list must belong to the superclass 'gaSequenceStep'."
    }
  }
)

setClass(
  "gaNonSequenceCondition",
  contains = c("gaDimensionOrMetricCondition", ".gaSimpleOrSequence")
)

setClass(
  "gaSegmentCondition",
  slots = c(
    conditionScope = "character"
  ),
  prototype = prototype(
    conditionScope = "sessions"
  ),
  contains = "list",
  validity = function(object) {
    if (!all_inherit(object@.Data, ".gaSimpleOrSequence")) {
      "All conditions within a gaSegmentCondition list must belong to the superclass '.gaSimpleOrSequence'."
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
    if (!all_inherit(object@.Data, "gaSegmentCondition")) {
      "All objects with a gaDynSegment list must belong to the class 'gaSegmentCondition'."
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

setClassUnion(".compoundExpr", c(
  ".expr", "orExpr", "andExpr",
  "gaMetExpr", "gaDimExpr", "mcfMetExpr", "mcfDimExpr", "rtMetExpr", "rtDimExpr"
))

# ---- dateRange ----

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

setClassUnion(".varList", c(".metrics", ".dimensions", ".sortBy"))

setValidity(".varList", function(object) {
  if (!all_inherit(object, ".var") & length(object@.Data) > 0) {
    "Must be a list containing objects of class .var"
  } else TRUE
})

setClass(
  "gaMetrics",
  prototype = prototype(
    list(new("gaMetVar"))
  ),
  contains = ".metrics"
)

setClass(
  "mcfMetrics",
  prototype = prototype(
    list(new("mcfMetVar"))
  ),
  contains = ".metrics"
)

setClass(
  "rtMetrics",
  prototype = prototype(
    list(new("rtMetVar"))
  ),
  contains = ".metrics"
)

setClass(
  "gaDimensions",
  prototype = prototype(
    list(new("gaDimVar"))
  ),
  contains = ".dimensions"
)

setClass(
  "mcfDimensions",
  prototype = prototype(
    list(new("mcfDimVar"))
  ),
  contains = ".dimensions"
)

setClass(
  "rtDimensions",
  prototype = prototype(
    list(new("rtDimVar"))
  ),
  contains = ".dimensions"
)

setClass("gaSortBy", contains = ".sortBy")
setClass("mcfSortBy", contains = ".sortBy")
setClass("rtSortBy", contains = ".sortBy")

setClassUnion(".gaVarList", c("gaMetrics", "gaDimensions", "gaSortBy"))

setValidity(".gaVarList", function(object) {
  if (!all_inherit(object, ".gaVar")) {
    "Must be a list containing objects of class .gaVar"
  } else TRUE
})

setClassUnion(".mcfVarList", c("mcfMetrics", "mcfDimensions", "mcfSortBy"))

setValidity(".mcfVarList", function(object) {
  if (!all_inherit(object, ".mcfVar")) {
    "Must be a list containing objects of class .mcfVar"
  } else TRUE
})

setClassUnion(".rtVarList", c("rtMetrics", "rtDimensions", "rtSortBy"))

setValidity(".rtVarList", function(object) {
  if (!all_inherit(object, ".rtVar")) {
    "Must be a list containing objects of class .rtVar"
  } else TRUE
})

# ---- Profile ID ----

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
