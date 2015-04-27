#' @importFrom methods setClass setClassUnion setValidity prototype
#' @importFrom stringr str_replace str_detect ignore.case
#' @importFrom assertthat validate_that
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# Class definitions for ganalytics
# --------------------------------

# ---- GA dimension and metric variables ----

setClass(
  Class = ".var",
  contains = "character",
  validity = function(object) {
    validate_that(length(object) == 1)
  }
)

setClass(
  Class = "gaMetVar",
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
  Class = "gaDimVar",
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
  Class = "mcfMetVar",
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
  Class = "mcfDimVar",
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
  Class = "rtMetVar",
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
  Class = "rtDimVar",
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

setClassUnion(
  name = ".gaVar",
  members = c("gaMetVar", "gaDimVar")
)

setClassUnion(
  name = ".mcfVar",
  members = c("mcfMetVar", "mcfDimVar")
)

setClassUnion(
  name = ".rtVar",
  members = c("rtMetVar", "rtDimVar")
)

setClassUnion(
  name = ".metVar",
  members = c("gaMetVar", "mcfMetVar", "rtMetVar")
)

setClassUnion(
  name = ".dimVar",
  members = c("gaDimVar", "mcfDimVar", "rtDimVar")
)

# ---- GA expression operators ----

setClass(
  Class = ".operator",
  contains = "character",
  prototype = prototype("=="),
  validity = function(object) {
    validate_that(length(object) == 1)
  }
)

setClass(
  Class = "gaMetOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$met)
  }
)

setClass(
  Class = "gaDimOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$dim)
  }
)

setClass(
  Class = "mcfMetOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$met)
  }
)

setClass(
  Class = "mcfDimOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$dim)
  }
)

setClass(
  Class = "rtMetOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$met)
  }
)

setClass(
  Class = "rtDimOperator",
  contains = ".operator",
  validity = function(object) {
    validate_that(object@.Data %in% kGaOps$dim)
  }
)

setClassUnion(
  name = ".gaOperator",
  members = c("gaMetOperator", "gaDimOperator")
)

setClassUnion(
  name = ".mcfOperator",
  members = c("mcfMetOperator", "mcfDimOperator")
)

setClassUnion(
  name = ".rtOperator",
  members = c("rtMetOperator", "rtDimOperator")
)

setClassUnion(
  name = ".dimOperator",
  members = c("gaDimOperator", "mcfDimOperator", "rtDimOperator")
)

setClassUnion(
  name = ".metOperator",
  members = c("gaMetOperator", "mcfMetOperator", "rtMetOperator")
)

# ---- GA expression operands ----

setClass(
  ".metOperand",
  contains = "numeric",
  validity = function(object) {
    validate_that(any(is.na(object)) == FALSE)
  }
)

setClass(
  ".dimOperand",
  contains = "character"
)

setClass(
  "gaMetOperand",
  contains = c(".metOperand"),
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

setClassUnion(
  ".gaOperand",
  c("gaMetOperand", "gaDimOperand")
)

setClassUnion(
  ".mcfOperand",
  c("mcfMetOperand", "mcfDimOperand")
)

setValidity(".mcfOperand", function(object) {
  validate_that(length(object) == 1)
})

setClassUnion(
  ".rtOperand",
  c("rtMetOperand", "rtDimOperand")
)

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

# ---- GA simple expressions -------------------------------------------------------

setClass(
  Class = ".gaExpr",
  slots = c(
    var = ".gaVar",
    operator = ".gaOperator",
    operand = ".gaOperand"
  )
)

setClass(
  Class = ".mcfExpr",
  slots = c(
    var = ".mcfVar",
    operator = ".mcfOperator",
    operand = ".mcfOperand"
  )
)

setClass(
  Class = ".rtExpr",
  slots = c(
    var = ".rtVar",
    operator = ".rtOperator",
    operand = ".rtOperand"
  )
)

setClass(
  Class = ".metExpr",
  slots = c(
    var = ".metVar",
    operator = ".metOperator",
    operand = ".metOperand"
  )
)

setClass(
  Class = ".dimExpr",
  slots = c(
    var = ".dimVar",
    operator = ".dimOperator",
    operand = ".dimOperand"
  )
)

setClass(
  Class = "gaMetExpr",
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

## TO DO move validation of <> expressions to gaSegMetExpr, as filters do not support this
## TO DO - test that filters do not support <> and []

setClass(
  Class = "gaSegMetExpr",
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

## TO DO - similarly to gaSegMetExpr, define a gaSegDimExpr for validating use of <> and [] expressions.

setClass(
  Class = "gaDimExpr",
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

# ---- GA 'AND' and 'OR' compound expressions -------------------------------

setClass(
  Class = "gaOr",
  contains = "list",
  validity = function(object) {
    if (all(sapply(object@.Data, inherits, ".expr"))) {
      TRUE
    } else {
      "gaOr must be a list containing objects that all inherit from the class .expr"
    }
  }
)

setClass(
  Class = "gaAnd",
  contains = "list",
  validity = function(object) {
    if (all(sapply(object@.Data, is, "gaOr"))) {
      TRUE
    } else {
      "gaAnd must be a list containing objects all of the class gaOr"
    }
  }
)

# ---- GA filter ----

setClass(
  ".filter",
  contains = "gaAnd",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to one class, i.e. either Metrics or Dimensions
    if (all(sapply(object@.Data, function(gaOr) {
      length(unique(sapply(gaOr, class))) == 1
    }))) {
      TRUE
    } else {
      return("An OR expression within a filter cannot mix metrics and dimensions.")
    }
  }
)

setClass(
  Class = "gaFilter",
  contains = ".filter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .gaExpr class
    if (all(sapply(object@.Data, function(gaOr) {
      all(sapply(gaOr, is, ".gaExpr"))
    }))) {
      TRUE
    } else {
      return("All expressions within a gaFilter must be of superclass .gaExpr")
    }
    
    if (all(sapply(object@.Data, function(gaOr) {
      sapply(gaOr, function(expr) {GaVar(expr) != "dateOfSession"})
    }))) {
      TRUE
    } else {
      return("Filters do not support the 'dateOfSession' dimension. Use 'ga:date' instead.")
    }
    
    if (all(sapply(object@.Data, function(gaOr) {
      sapply(gaOr, function(expr) {!(Operator(expr) %in% c("<>", "[]"))})
    }))) {
      TRUE
    } else {
      return("Filters do not support <> and [] operators.")
    }
  }
)

setClass(
  Class = "mcfFilter",
  contains = ".filter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .mcfExpr class
    if (all(sapply(object@.Data, function(gaOr) {
      all(sapply(gaOr, is, ".mcfExpr"))
    }))) {
      TRUE
    } else {
      return("All expressions within a mcfFilter must be of superclass .mcfExpr")
    }
  }
)

setClass(
  Class = "rtFilter",
  contains = ".filter",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to .mcfExpr class
    if (all(sapply(object@.Data, function(gaOr) {
      all(sapply(gaOr, is, ".rtExpr"))
    }))) {
      TRUE
    } else {
      return("All expressions within a rtFilter must be of superclass .rtExpr")
    }
  }
)

# ---- GA dynamic and pre-defined segments ----

## TO DO - rename these classes to be clear they are segmentation classes.

setClass(
  Class = "gaDimensionOrMetricCondition",
  contains = "gaAnd",
  validity = function(object) {
    if (all(sapply(object@.Data, function(gaOr) {
      sapply(gaOr, function(expr) {
        if (Operator(expr) == "<>" & GaVar(expr) == "dateOfSession") {
          (Operand(expr)[2] - Operand(expr)[1] + 1) <= 31
        } else TRUE
      })
    }))) {
      TRUE
    } else {
      return("The maximum date range for dateOfSession is 31 days.")
    }
    
    if (all(sapply(object@.Data, function(gaOr) {
      sapply(gaOr, function(expr) {
        if (GaVar(expr) == "dateOfSession") {
          Operator(expr) == "<>"
        } else TRUE
      })
    }))) {
      TRUE
    } else {
      return("The dateOfSession dimension can only be used with a <> operator.")
    }
  }
)

setClass(
  Class = ".gaSimpleOrSequence",
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
  Class = "gaSequenceStep",
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
  Class = "gaSequenceCondition",
  contains = c("list", ".gaSimpleOrSequence"),
  validity = function(object) {
    if (all(sapply(object@.Data, inherits, "gaSequenceStep"))) {
      TRUE
    } else {
      "All conditions within a sequence list must belong to the superclass 'gaSequenceStep'."
    }
  }
)

setClass(
  Class = "gaNonSequenceCondition",
  contains = c("gaDimensionOrMetricCondition", ".gaSimpleOrSequence")
)

setClass(
  Class = "gaSegmentCondition",
  slots = c(
    conditionScope = "character"
  ),
  prototype = prototype(
    conditionScope = "sessions"
  ),
  contains = "list",
  validity = function(object) {
    if (!all(sapply(object@.Data, inherits, ".gaSimpleOrSequence"))) {
      "All conditions within a gaSegmentCondition list must belong to the superclass '.gaSimpleOrSequence'."
    } else if (length(object@conditionScope) != 1) {
      "Slot 'conditionScope' must be of length 1."
    } else if (!(object@conditionScope %in% c("users", "sessions"))) {
      "Slot 'conditionScope' must be either 'users' or 'sessions'."
    } else TRUE
  }
)

setClass(
  Class = "gaDynSegment",
  contains = "list",
  validity = function(object) {
    if (!all(sapply(object@.Data, function(x) {inherits(x, "gaSegmentCondition")}))) {
      "All objects with a gaDynSegment list must belong to the class 'gaSegmentCondition'."
    } else if (identical(nchar(as(object, "character")) > 1024, TRUE)) {
      "The maximum expression length for dimension conditions is 1024 characters."
    } else if (sum(rapply(object, is, class2 = ".gaExpr")) > 10) {
      "A maximum of 10 dimension or metric conditions per segment."
    } else TRUE
  }
)

setClass(
  Class = "gaSegmentId",
  contains = "character",
  validity = function(object) {
    pattern <- "^gaid::\\-?[0-9]+$"
    if (length(object) != 1) {
      "gaSegmentId must be a character vector of length 1"
    } else if (!grepl(pattern = pattern, x = object@.Data)) {
      paste("gaSegmentId must match the regular expression ", pattern, sep = "")
    } else TRUE
  }
)

setClassUnion(
  name = ".gaSegment",
  members = c("gaDynSegment", "gaSegmentId")
)

# ---- Simple and compound expression class union ----

setClassUnion(
  name = ".compoundExpr",
  members = c(".expr", ".gaExpr", ".mcfExpr", "gaOr", "gaAnd", "gaFilter", "gaSequenceStep", "gaNonSequenceCondition")
  # "gaFilter", "gaSequenceStep" and "gaNonSequenceCondition" already inherit from gaAnd
)

# ---- GA query dimensions, metrics, and sortby lists ----

setClass(
  Class = "gaDateRange",
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

setClass(
  Class = ".metrics",
  contains = "list",
  validity = function(object) {
    if (length(object) > kGaMax$metrics) {
      paste("Maximum of", kGaMax$metrics, "metrics allowed.", sep = " ")
    } else TRUE
  }
)

setClass(
  Class = "gaMetrics",
  contains = ".metrics",
  prototype = prototype(
    list(new("gaMetVar"))
  ),
  validity = function(object) {
    if (!all(sapply(object, is, "gaMetVar"))) {
      "Must be a list containing objects of class gaMetVar"
    } else TRUE
  }
)

setClass(
  Class = "mcfMetrics",
  contains = ".metrics",
  prototype = prototype(
    list(new("mcfMetVar"))
  ),
  validity = function(object) {
    if (!all(sapply(object, is, "mcfMetVar"))) {
      "Must be a list containing objects of class mcfMetVar"
    } else TRUE
  }
)

setClass(
  Class = "rtMetrics",
  contains = ".metrics",
  prototype = prototype(
    list(new("rtMetVar"))
  ),
  validity = function(object) {
    if (!all(sapply(object, is, "rtMetVar"))) {
      "Must be a list containing objects of class rtMetVar"
    } else TRUE
  }
)

setClass(
  Class = ".dimensions",
  contains = "list",
  validity = function(object) {
    if (length(object) > kGaMax$dimensions) {
      paste("Maximum of", kGaMax$dimensions, "dimensions allowed.", sep = " ")
    } else TRUE
  }
)

setClass(
  Class = "gaDimensions",
  prototype = prototype(
    list(new("gaDimVar"))
  ),
  contains = ".dimensions",
  validity = function(object) {
    if (!all(sapply(object, is, "gaDimVar"))) {
      "Must be a list containing objects of class gaDimVar"
    } else TRUE
  }
)

setClass(
  Class = "mcfDimensions",
  prototype = prototype(
    list(new("mcfDimVar"))
  ),
  contains = ".dimensions",
  validity = function(object) {
    if (!all(sapply(object, is, "mcfDimVar"))) {
      "Must be a list containing objects of class mcfDimVar"
    } else TRUE
  }
)

setClass(
  Class = "rtDimensions",
  prototype = prototype(
    list(new("rtDimVar"))
  ),
  contains = ".dimensions",
  validity = function(object) {
    if (!all(sapply(object, is, "rtDimVar"))) {
      "Must be a list containing objects of class rtDimVar"
    } else TRUE
  }
)

setClass(
  Class = ".sortBy",
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

setClass(
  Class = "gaSortBy",
  contains = ".sortBy",
  validity = function(object) {
    if (!all(sapply(object@.Data, is, ".gaVar"))) {
      "Must be a list containing objects of class .gaVar"
    } else TRUE
  }
)

setClass(
  Class = "mcfSortBy",
  contains = ".sortBy",
  validity = function(object) {
    if (!all(sapply(object@.Data, is, ".mcfVar"))) {
      "Must be a list containing objects of class .mcfVar"
    } else TRUE
  }
)

setClass(
  Class = "rtSortBy",
  contains = ".sortBy",
  validity = function(object) {
    if (!all(sapply(object@.Data, is, ".rtVar"))) {
      "Must be a list containing objects of class .rtVar"
    } else TRUE
  }
)

setClassUnion(
  name = ".gaVarList",
  members = c(".metrics", ".dimensions", ".sortBy"),
)

# ---- Ga Profile ID ----

setClass(
  Class = "gaProfileId",
  contains = "character",
  validity = function(object) {
    if (all(str_detect(object, "^ga:[0-9]+$"))) {
      TRUE
    } else {
      "gaProfileId must be an string of digits preceeded by 'ga:'"
    }
  }
)

# -- GA query construct ----

setClass(
  Class = ".query",
  slots = c(
    viewId = "gaProfileId",
    dateRange = "gaDateRange",
    metrics = ".metrics",
    dimensions = ".dimensions",
    sortBy = ".sortBy",
    filters = ".filter",
    samplingLevel = "character",
    maxResults = "numeric",
    creds = "list"
  ),
  prototype = prototype(
    dateRange = new("gaDateRange"),
    samplingLevel = "DEFAULT",
    maxResults = kGaMaxResults,
    creds = list()
  ),
  validity = function(object) {
    valid <- validate_that(
      length(object@maxResults) == 1,
      object@maxResults >= 1,
      length(object@samplingLevel) == 1
    )
    if (valid == TRUE) {
      if (object@maxResults > kGaMaxRows) {
        "maxResults cannot be greater than 1,000,000"
      } else if (!all(object@sortBy %in% union(object@metrics, object@dimensions))) {
        "sortBy must contain varNames also used as metrics and/or dimensions"
      } else if (!(object@samplingLevel %in% samplingLevel_levels)) {
        paste("samplingLevel must be one of:", paste(samplingLevel_levels, collapse = ", "))
      } else TRUE
    } else valid
  }
)

setClass(
  Class = "gaQuery",
  slots = c(
    metrics = "gaMetrics",
    dimensions = "gaDimensions",
    sortBy = "gaSortBy",
    filters = "gaFilter",
    segment = ".gaSegment"
  ),
  prototype = prototype(
    metrics = new("gaMetrics"),
    dimensions = new("gaDimensions"),
    sortBy = new("gaSortBy")
  ),
  contains = ".query"
)

setClass(
  Class = "mcfQuery",
  slots = c(
    metrics = "mcfMetrics",
    dimensions = "mcfDimensions",
    sortBy = "mcfSortBy",
    filters = "mcfFilter"
  ),
  prototype = prototype(
    metrics = new("mcfMetrics"), # To be changed to mcfMetrics when class is defined
    dimensions = new("mcfDimensions"), # To be changed to mcfDimensions when class is defined
    sortBy = new("mcfSortBy")
  ),
  contains = ".query"
)

setClass(
  Class = "rtQuery",
  slots = c(
    metrics = "rtMetrics",
    dimensions = "rtDimensions",
    sortBy = "rtSortBy",
    filters = "rtFilter"
  ),
  prototype = prototype(
    metrics = new("rtMetrics"), # To be changed to mcfMetrics when class is defined
    dimensions = new("rtDimensions"), # To be changed to mcfDimensions when class is defined
    sortBy = new("rtSortBy")
  ),
  contains = ".query"
)
