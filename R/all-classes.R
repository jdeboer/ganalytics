#' @importFrom methods setClass setClassUnion setValidity prototype
#' @importFrom stringr str_replace str_detect ignore.case
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# Class definitions for ganalytics
# --------------------------------

# ---- GA dimension and metric variables ----

setClass(
  Class = "gaMetVar",
  prototype = prototype("ga:sessions"),
  contains = "character",
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
  contains = "character",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kGaVars$dims)) {
      TRUE
    } else {
      paste("Invalid GA dimension name", object@.Data, sep = ": ")
    }
  }
)

setClassUnion(
  name = ".gaVar",
  members = c("gaMetVar", "gaDimVar")
)

setValidity(
  Class = ".gaVar",
  method = function(object) {
    if(length(object) == 1) {
      TRUE
    } else {
      "gaVar must be a of length 1."
    }
  }
)

# ---- GA expression operators ----

setClass(
  Class = "gaMetOperator",
  contains = "character",
  prototype = prototype("=="),
  validity = function(object) {
    if (object@.Data %in% kGaOps$met) {
      TRUE
    } else {
      paste("Invalid metric operator", object@.Data, sep = ": ")
    }
  }
)

setClass(
  Class = "gaDimOperator",
  contains = "character",
  prototype = prototype("=="),
  validity = function(object) {
    if (object@.Data %in% kGaOps$dim) {
      TRUE
    } else {
      paste("Invalid dimension operator", object@.Data, sep = ": ")
    }
  }
)

setClassUnion(
  name = ".gaOperator",
  members = c("gaMetOperator", "gaDimOperator")
)

setValidity(
  Class = ".gaOperator",
  method = function(object) {
    if(length(object) == 1) {
      TRUE
    } else {
      "gaOperator's must be character vector of length 1"
    }
  }
)

# ---- GA expression operands ----

setClass(
  Class = "gaMetOperand",
  contains = "numeric",
  validity = function(object) {
    if (length(object) %in% c(1, 2)) {
      return("A gaMetOperand must be of length 1 or 2")
    } else TRUE
  }
)

setClass(
  Class = "gaDimOperand",
  contains = "character",
  validity = function(object) {
    if (length(object) < 1) {
      return("A gaDimOperand must be of at least length 1")
    } else TRUE
  }
)

setClassUnion(
  name = ".gaOperand",
  members = c("gaMetOperand", "gaDimOperand")
)

# ---- GA simple expressions -------------------------------------------------------

setClass(
  Class = ".gaExpr",
  slots = c(
    gaVar = ".gaVar",
    gaOperator = ".gaOperator",
    gaOperand = ".gaOperand"
  ),
  contains = "VIRTUAL"
)

setClass(
  Class = "gaMetExpr",
  contains = ".gaExpr",
  validity = function(object) {
    if (!class(object@gaVar)=="gaMetVar") {
      return("gaVar must be of class gaMetVar")
    } else if (!class(object@gaOperator)=="gaMetOperator") {
      return("gaOperator must be of class gaMetOperator")
    } else if (!class(object@gaOperand)=="gaMetOperand") {
      return("gaOperand must be of class gaMetOperand")
    } else if (!(
      length(object@gaOperand) == 1 &
        object@gaOperator != "<>"
      )) {
      return("gaOperand must be of length 1 unless using a range operator '<>'.")
    } else if (!(
      length(object@gaOperand) == 2 &
        object@gaOperator == "<>"
    )) {
      return("gaOperand must be of length 2 when using a range operator '<>'.")
    } else TRUE
  }
)

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
    if (!length(object@metricScope) == 1) {
      return("metricScope must be of length 1.")
    } else if (!(object@metricScope %in% c("perUser", "perSession", "perHit"))) {
      return("metricScope must be one of 'perUser', 'perSession' or 'perHit'.")
    } else TRUE
  }
)

#   #{dimensionOrMetricName}<>{minValue}_{maxValue} #For metrics or numerical dimensions, also dates
#   #{dimensionName}[]{value1}|{value2}|...
#   #A maximum of 10 values per in-list dimension condition is allowed. Only for dimensions
#   #dateOfSession<>2014-05-20_2014-05-30
#   dateOfSession is a special dimension

setClass(
  Class = "gaDimExpr",
  contains = ".gaExpr",
  validity = function(object) {
    if (!class(object@gaVar)=="gaDimVar") {
      return("gaVar must be of class gaDimVar")
    } else if (!class(object@gaOperator)=="gaDimOperator") {
      return("gaOperator must be of class gaDimOperator")
    } else if (!class(object@gaOperand)=="gaDimOperand") {
      return("gaOperand must be of class gaDimOperand")
    } else if (!(
      length(object@gaOperand) == 1 |
        object@gaOperator %in% c("<>", "[]")
      )) {
      return("gaOperand must be of length 1 unless using a range '<>' or list '[]' operator.")
    } else if (!(
      length(object@gaOperand) <= 2 |
        object@gaOperator == "[]"
      )) {
      return("gaOperand may only be greater than length 2 if using a list operator '[]'.")
    } else if (GaIsRegEx(object@gaOperator) & nchar(object@gaOperand) > 128) {
      return(
        paste("Regular expressions in GA Dimension Expressions cannot exceed 128 chars. Length", nchar(object@gaOperand), sep = " = ")
      )
    } else if (object@gaOperator %in% c("!=", "==", "<>", "[]")) {
      return(
        ValidGaOperand(object@gaVar, object@gaOperand)
      )
    } else TRUE
  }
)

# ---- GA 'AND' and 'OR' compound expressions -------------------------------

setClass(
  Class = "gaOr",
  contains = "list",
  validity = function(object) {
    if (!all(sapply(object@.Data, function(x) {
      inherits(x, ".gaExpr")
      }))) {
      "gaOr must be a list containing objects that all inherit from the class .gaExpr"
    } else {
      TRUE
    }
  }
)

setClass(
  Class = "gaAnd",
  contains = "list",
  validity = function(object) {
    if (!all(sapply(object@.Data, function(x) {
        class(x) == "gaOr"
      }))) {
      "gaAnd must be a list containing objects all of the class gaOr"
    } else {
      TRUE
    }
  }
)

# ---- Simple and compound expression class union ----

setClassUnion(
  name = ".gaCompoundExpr",
  members = c(".gaExpr", "gaOr", "gaAnd")
)

setClassUnion(
  name = ".gaLogical",
  members = c(".gaOperator",".gaCompoundExpr")
)

# ---- GA filter ----

setClass(
  Class = "gaFilter",
  contains = "gaAnd",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to one class, i.e. either Metrics or Dimensions
    if (
      all(
        sapply(
          object@.Data,
          function(gaOr) {
            length(unique(sapply(gaOr, class))) == 1
          }
        )
      )
    ) {
      TRUE
    } else {
      "An OR expression within a filter cannot mix metrics and dimensions."
    }
  }
)

# ---- GA dynamic and pre-defined segments ----

setClass(
  Class = ".gaDimensionOrMetricConditions",
  slots = c(
    negation = "logical"
  ),
  prototype = prototype(
    negation = FALSE
  ),
  contains = "VIRTUAL",
  validity = function(object) {
    if (length(object@negation) == 1) {
      TRUE
    } else {
      "Slot negation must be of length 1."
    }
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
  contains = "gaAnd",
  validity = function(object) {
    if (length(object@immediatelyPrecedes) == 1) {
      TRUE
    } else {
      "immediatelyPrecedes must be of length 1."
    }
  }
)

setClass(
  Class = "gaSequenceCondition",
  contains = c("list", ".gaDimensionOrMetricConditions"),
  validity = function(object) {
    if (all(sapply(object@.Data, function(x) {
      inherits(x, "gaSequenceStep")
    }))) {
      TRUE
    } else {
      "All conditions within a sequence list must belong to the superclass gaSequenceStep."
    }
  }
)

setClass(
  Class = "gaNonSequenceCondition",
  contains = c("gaAnd", ".gaDimensionOrMetricConditions")
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
    if (!all(sapply(object@.Data, function(x) {
      inherits(x, ".gaDimensionOrMetricConditions")
      }))) {
      return("All conditions within a gaSegmentCondition list must belong to the superclass .gaDimensionOrMetricConditions.")
    } else if (length(object@conditionScope) != 1) {
      return("Slot 'conditionScope' must be of length 1.")
    } else if (!(object@conditionScope %in% c("users", "sessions"))) {
      return("Slot 'conditionScope' must be either 'users' or 'sessions'.")
    } else TRUE
  }
)

setClass(
  Class = "gaDynSegment",
  contains = "list",
  validity = function(object) {
    if (all(sapply(object@.Data, function(x) {inherits(x, "gaSegmentCondition")}))) {
      TRUE
    } else {
      "All objects with a gaDynSegment list must belong to the class gaSegmentCondition."
    } 
  }
)

setClass(
  Class = "gaSegmentId",
  contains = "character",
  validity = function(object) {
    pattern <- "^gaid::\\-?[0-9]+$"
    if (length(object) != 1) {
      return("gaSegmentId must be a character vector of length 1")
    } else if (!grepl(pattern = pattern, x = object@.Data)) {
      return(
        paste("gaSegmentId must match the regular expression ", pattern, sep = "")
      )
    } else TRUE
  }
)

setClassUnion(
  name = ".gaSegment",
  members = c("gaDynSegment", "gaSegmentId")
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
      return("startDate and endDate must be the same length")
    } else if (all(object@startDate > object@endDate)) {
      return("endDate cannot be before startDate")
    } else if (object@startDate < kGaDateOrigin) {
      return("Start date cannot preceed Google Analytics launch date 2005-01-01")
    } else TRUE
  }
)

setClass(
  Class = "gaMetrics",
  prototype = prototype(
    list(new("gaMetVar"))
  ),
  contains = "list",
  validity = function(object) {
    if (!all(sapply(object, function(gaVar) {
        class(gaVar) == "gaMetVar"
      }))) {
      return("Must be a list containing objects of class gaMetVar")
    } else if (length(object) > kGaMax$metrics) {
      return(
        paste("Maximum of", kGaMax$metrics, "metrics allowed.", sep = " ")
      )
    } else TRUE
  }
)

setClass(
  Class = "gaDimensions",
  prototype = prototype(
    list(new("gaDimVar"))
  ),
  contains = "list",
  validity = function(object) {
    if (!all(sapply(object, function(gaVar) {
      class(gaVar) == "gaDimVar"
    }))) {
      return("Must be a list containing objects of class gaDimVar")
    } else if (length(object) > kGaMax$dimensions) {
      return(
        paste("Maximum of", kGaMax$dimensions, "dimensions allowed.", sep = " ")
      )
    } else TRUE
  }
)

setClass(
  Class = "gaSortBy",
  slots = c(
    desc = "logical"
  ),
  prototype = prototype(
    list(),
    desc = logical()
  ),
  contains = "list",
  validity = function(object) {
    if (!all(sapply(object@.Data, function(gaVar) {
        inherits(gaVar, ".gaVar")
      }))) {
      return("Must be a list containing objects of class .gaVar")
    } else if (length(object@.Data) != length(object@desc)) {
      return("List vector and desc vector must be of equal lengths")
    } else TRUE
  }
)

setClassUnion(
  name = ".gaVarList",
  members = c("gaMetrics", "gaDimensions", "gaSortBy"),
)

# ---- Ga Profile ID ----


setClass(
  Class = "gaProfileId",
  contains = "character",
  validity = function(object) {
    if (!all(sapply(object, function(profileId) {
        grepl(pattern = "^ga:[0-9]+$",  x = profileId)
    }))) {
      return("gaProfileId must be an string of digits preceeded by 'ga:'")      
    } else TRUE
  }
)

# -- GA query construct ----

samplingLevel_levels <- c("DEFAULT", "FASTER", "HIGHER_PRECISION")

setClass(
  Class = "gaQuery",
  slots = c(
    profileId = "gaProfileId",
    dateRange = "gaDateRange",
    metrics = "gaMetrics",
    dimensions = "gaDimensions",
    sortBy = "gaSortBy",
    filters = "gaFilter",
    segment = ".gaSegment",
    samplingLevel = "character",
    maxResults = "numeric",
    creds = "list"
  ),
  prototype = prototype(
    dateRange = new("gaDateRange"),
    metrics = new("gaMetrics"),
    dimensions = new("gaDimensions"),
    sortBy = new("gaSortBy"),
    samplingLevel = "DEFAULT",
    maxResults = kGaMaxResults,
    creds = list()
  ),
  validity = function(object) {
    if (length(object@maxResults) != 1) {
      return("maxResults must be of length 1")
    } else if (object@maxResults < 1) {
      return("maxResults must be at least 1")
    } else if (object@maxResults > kGaMaxRows) {
      return("maxResults cannot be greater than 1,000,000")
    } else if (!all(object@sortBy %in% union(object@metrics, object@dimensions))) {
      return("sortBy must contain varNames also used as metrics and/or dimensions")
    } else if (length(object@samplingLevel) != 1) {
      return("samplingLevel must be of length 1")
    } else if (!(object@samplingLevel %in% samplingLevel_levels)) {
      return(paste("samplingLevel must be one of:", samplingLevel_levels))
    } else TRUE
  }
)

setClassUnion(
  name = ".gaUrlClasses",
  members = c(
    #".gaCompoundExpr",
    ".gaExpr", "gaOr", "gaAnd", "gaDynSegment",
    
    #".gaVarList",
    "gaMetrics", "gaDimensions", "gaSortBy",
    
    ".gaVar",
    ".gaOperator",
    ".gaOperand",
    ".gaSegment",
    "gaFilter",
    "gaProfileId",
    "Date",
    "gaQuery"
  )
)

setClass(
  Class = "utf8",
  contains = "character"
)
