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

setClass(
  Class = "mcfMetVar",
  prototype = prototype("mcf:totalConversions"),
  contains = "character",
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
  contains = "character",
  validity = function(object) {
    if (IsVarMatch(object@.Data, kMcfVars$dims)) {
      TRUE
    } else {
      paste("Invalid MCF dimension name", object@.Data, sep = ": ")
    }
  }
)

setClassUnion(
  name = ".mcfVar",
  members = c("mcfMetVar", "mcfDimVar")
)

setClassUnion(
  name = ".gaVar",
  members = c("gaMetVar", "gaDimVar")
)

setClassUnion(
  name = ".var",
  members = c(".gaVar", ".mcfVar")
)

setValidity(
  Class = ".var",
  method = function(object) {
    validate_that(length(object) == 1)
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
    validate_that(length(object) == 1)
  }
)

# ---- GA expression operands ----

setClass(
  Class = "gaMetOperand",
  contains = "numeric",
  validity = function(object) {
    if (!(length(object) %in% c(1, 2))) {
      "A gaMetOperand must be of length 1 or 2"
    } else if (any(is.na(object))) {
      "A gaMetOperand connot contain NA values"
    } else if (length(object) == 2) {
      if (object[1] > object[2]) {
        "The first value in a range must not be greater than the second"
      } else TRUE
    } else TRUE
  }
)

setClass(
  Class = "gaDimOperand",
  contains = "character",
  validity = function(object) {
    validate_that(
      length(object) >= 1,
      length(object) <= 10
    )
  }
)

setClassUnion(
  name = ".gaOperand",
  members = c("gaMetOperand", "gaDimOperand")
)

# ---- GA simple expressions -------------------------------------------------------

# Need to define classes for expressions using exclusive MCF dimensions or MCF metrics

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
    valid <- validate_that(
      is(object@gaVar, "gaMetVar"),
      is(object@gaOperator, "gaMetOperator"),
      is(object@gaOperand, "gaMetOperand")
    )
    if (valid == TRUE) {
      if (object@gaOperator != "<>") {
        if (length(object@gaOperand) != 1) {
          "gaOperand must be of length 1 unless using a range operator '<>'."
        } else TRUE
      } else {
        if (length(object@gaOperand) != 2) {
          "gaOperand must be of length 2 when using a range operator '<>'."
        } else TRUE
      }
    } else valid
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
    validate_that(
      length(object@metricScope) == 1,
      object@metricScope %in% c("perUser", "perSession", "perHit")
    )
  }
)

setClass(
  Class = "gaDimExpr",
  contains = ".gaExpr",
  validity = function(object) {
    valid <- validate_that(
      is(object@gaVar, "gaDimVar"),
      is(object@gaOperator, "gaDimOperator"),
      is(object@gaOperand, "gaDimOperand")
    )
    if (valid == TRUE) {
      if (object@gaOperator == "<>") {
        rangeDimVars <- unlist(kGaDimTypes[c("nums", "dates", "orderedIntFactors")], use.names = FALSE)
        if (!(object@gaVar %in% rangeDimVars)) {
          return("A range operator only supports numerical dimensions or metrics")
        }
      }
      if (!(length(object@gaOperand) == 1 | object@gaOperator %in% c("<>", "[]"))) {
        return("gaOperand must be of length 1 unless using a range '<>' or list '[]' operator.")
      } else if (!(length(object@gaOperand) <= 2 | object@gaOperator == "[]")) {
        return("gaOperand may only be greater than length 2 if using a list operator '[]'.")
      } else if (GaIsRegEx(object@gaOperator)) {
        if (nchar(object@gaOperand) > 128) {
          return(paste0("Regular expressions in GA Dimension Expressions cannot exceed 128 chars. Length = ", nchar(object@gaOperand)))
        }
      }
      if (object@gaOperator %in% c("!=", "==", "<>", "[]")) {
        ValidGaOperand(object@gaVar, object@gaOperand)
      } else TRUE
    } else valid
  }
)

# ---- GA 'AND' and 'OR' compound expressions -------------------------------

setClass(
  Class = "gaOr",
  contains = "list",
  validity = function(object) {
    if (all(sapply(object@.Data, inherits, ".gaExpr"))) {
      TRUE
    } else {
      "gaOr must be a list containing objects that all inherit from the class .gaExpr"
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
  Class = "gaFilter",
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

    if (all(sapply(object@.Data, function(gaOr) {
      sapply(gaOr, function(expr) {GaVar(expr) != "dateOfSession"})
    }))) {
      TRUE
    } else {
      return("Filters do not support the 'dateOfSession' dimension. Use 'ga:date' instead.")
    }
    
    if (all(sapply(object@.Data, function(gaOr) {
      sapply(gaOr, function(expr) {!(GaOperator(expr) %in% c("<>", "[]"))})
    }))) {
      TRUE
    } else {
      return("Filters do not support <> and [] operators.")
    }
  }
)

# ---- GA dynamic and pre-defined segments ----

setClass(
  Class = "gaDimensionOrMetricCondition",
  contains = "gaAnd",
  validity = function(object) {
    if (all(sapply(object@.Data, function(gaOr) {
      sapply(gaOr, function(expr) {
        if (GaOperator(expr) == "<>" & GaVar(expr) == "dateOfSession") {
          (GaOperand(expr)[2] - GaOperand(expr)[1] + 1) <= 31
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
          GaOperator(expr) == "<>"
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
  name = ".gaCompoundExpr",
  members = c(".gaExpr", "gaOr", "gaAnd", "gaFilter", "gaSequenceStep", "gaNonSequenceCondition")
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
  Class = ".gaMetrics",
  prototype = prototype(
    list(new("gaMetVar"))
  ),
  contains = "list",
  validity = function(object) {
    if (length(object) > kGaMax$metrics) {
      paste("Maximum of", kGaMax$metrics, "metrics allowed.", sep = " ")
    } else TRUE
  }
)

setClass(
  Class = "gaMetrics",
  contains = ".gaMetrics",
  validity = function(object) {
    if (!all(sapply(object, is, "gaMetVar"))) {
      "Must be a list containing objects of class gaMetVar"
    } else TRUE
  }
)

setClass(
  Class = "mcfMetrics",
  contains = ".gaMetrics",
  validity = function(object) {
    if (!all(sapply(object, is, "mcfMetVar"))) {
      "Must be a list containing objects of class mcfMetVar"
    } else TRUE
  }
)

setClass(
  Class = ".gaDimensions",
  prototype = prototype(
    list(new("gaDimVar"))
  ),
  contains = "list",
  validity = function(object) {
    if (!all(sapply(object, is, "gaDimVar"))) {
      "Must be a list containing objects of class gaDimVar"
    } else if (length(object) > kGaMax$dimensions) {
      paste("Maximum of", kGaMax$dimensions, "dimensions allowed.", sep = " ")
    } else TRUE
  }
)

setClass(
  Class = "gaDimensions",
  contains = ".gaDimensions",
  validity = function(object) {
    if (!all(sapply(object, is, "gaDimVar"))) {
      "Must be a list containing objects of class gaDimVar"
    } else TRUE
  }
)

setClass(
  Class = "mcfDimensions",
  contains = ".gaDimensions",
  validity = function(object) {
    if (!all(sapply(object, is, "mcfDimVar"))) {
      "Must be a list containing objects of class mcfDimVar"
    } else TRUE
  }
)

setClass(
  Class = ".gaSortBy",
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
  contains = ".gaSortBy",
  validity = function(object) {
    if (!all(sapply(object@.Data, is, ".gaVar"))) {
      "Must be a list containing objects of class .gaVar"
    } else TRUE
  }
)

setClass(
  Class = "mcfSortBy",
  contains = ".gaSortBy",
  validity = function(object) {
    if (!all(sapply(object@.Data, is, ".mcfVar"))) { # Need to define .mcfVar
      "Must be a list containing objects of class .mcfVar"
    } else TRUE
  }
)

setClassUnion(
  name = ".gaVarList",
  members = c(".gaMetrics", ".gaDimensions", ".gaSortBy"),
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
  Class = ".gaQuery",
  slots = c(
    profileId = "gaProfileId",
    dateRange = "gaDateRange",
    metrics = ".gaMetrics",
    dimensions = ".gaDimensions",
    sortBy = ".gaSortBy",
    filters = "gaFilter",
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
    segment = ".gaSegment"
  ),
  prototype = prototype(
    metrics = new("gaMetrics"),
    dimensions = new("gaDimensions"),
    sortBy = new("gaSortBy")
  ),
  contains = ".gaQuery"
)

setClass(
  Class = "mcfQuery",
  prototype = prototype(
    metrics = new("mcfMetrics"), # To be changed to mcfMetrics when class is defined
    dimensions = new("mcfDimensions"), # To be changed to mcfDimensions when class is defined
    sortBy = new("mcfSortBy")
  ),
  contains = ".gaQuery"
)
