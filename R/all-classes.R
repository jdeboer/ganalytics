#' @include ganalytics-package.R
#' @include ValidGaOperand.R
NULL

# Class definitions for ganalytics
# --------------------------------

# ---- GA dimension and metric variables ----

#' @export
setClass(
  Class = "gaMetVar",
  prototype = prototype("ga:visits"),
  contains = "character",
  validity = function(object) {
    if (tolower(object@.Data) %in% tolower(kGaVars$mets)) {
      return(TRUE)
    } else {
      paste("Invalid GA metric name", object@.Data, sep = ": ")
    }
  }
)

#' @export
setClass(
  Class = "gaDimVar",
  prototype = prototype("ga:date"),
  contains = "character",
  validity = function(object) {
    if (tolower(object@.Data) %in% tolower(kGaVars$dims)) {
      return(TRUE)
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
  Class=".gaVar",
  method=function(object) {
    if(length(object) == 1) {
      return(TRUE)
    } else {
      return("gaVar's must be a character vector of length 1")
    }
  }
)

# ---- GA expression operators ----

#' @export
setClass(
  Class = "gaMetOperator",
  contains = "character",
  prototype = prototype("=="),
  validity = function(object) {
    if (object@.Data %in% kGaOps$met) {
      return(TRUE)
    } else {
      paste("Invalid metric operator", object@.Data, sep = ": ")
    }
  }
)

#' @export
setClass(
  Class = "gaDimOperator",
  contains = "character",
  prototype = prototype("=="),
  validity = function(object) {
    if (object@.Data %in% kGaOps$dim) {
      return(TRUE)
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
      return(TRUE)
    } else {
      return("gaOperator's must be character vector of length 1")
    }
  }
)

# ---- GA expression operands ----

#' @export
setClass(
  Class = "gaMetOperand",
  contains = "numeric"
)

#' @export
setClass(
  Class = "gaDimOperand",
  contains = "character"
)

setClassUnion(
  name = ".gaOperand",
  members = c("gaMetOperand", "gaDimOperand")
)

setValidity(
  Class = ".gaOperand",
  method = function(object) {
    if(length(object) == 1) {
      return(TRUE)
    } else {
      return(".gaOperand must be a vector of length 1")
    }
  }
)

# ---- GA simple expressions -------------------------------------------------------

setClass(
  Class = ".gaExpr",
  representation = representation(
    gaVar = ".gaVar",
    gaOperator = ".gaOperator",
    gaOperand = ".gaOperand"
  ),
  contains = "VIRTUAL"
)

#' @export
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
    } else {
      return(TRUE)
    }
  }
)

#' @export
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
    } else if (GaIsRegEx(object@gaOperator)) {
      regExLen <- nchar(object@gaOperand)
      if (regExLen > 128) {
        return(
          paste("Regular expressions in GA Dimension Expressions cannot exceed 128 chars. Length", regExLen, sep = " = ")
        )
      }
    } else if (object@gaOperator %in% c("!=", "==")) {
      if (ValidGaOperand(object@gaVar, object@gaOperand)) {
        return(TRUE)
      }
    } else {
      return(TRUE)
    }
  }
)

# ---- GA 'AND' and 'OR' compound expressions -------------------------------

#' @export
setClass(
  Class = "gaOr",
  contains = "list",
  # A object of class gaOr must be a list containing
  # objects from the superclass .gaExpr
  # i.e. it must contain gaDimExprs or gaMetExprs, or both
  validity = function(object) {
    if (
      !all(
        sapply(
          X = object@.Data,
          FUN = function(x) {
            inherits(x, ".gaExpr")
          }
        )
      )
    ) {
      return("gaOr must be a list containing objects that all inherit from the class .gaExpr")
    } else {
      return(TRUE)
    }
  }
)

#' @export
setClass(
  Class = "gaAnd",
  contains = "list",
  validity = function(object) {
    if (
      all(
        sapply(
          X = object@.Data,
          FUN = function(x) {
            class(x) == "gaOr"
          }
        )
      )
    ) {
      return(TRUE)
    } else {
      return("gaAnd must be a list containing objects all of the class gaOr")
    }
  }
)

# ---- Simple and compound expression class union ----

setClassUnion(
  name = ".gaCompoundExpr",
  members = c(".gaExpr", "gaOr", "gaAnd")
)

# ---- GA filter ----

#' @export
setClass(
  Class = "gaFilter",
  contains = "gaAnd",
  validity = function(object) {
    ## Check that single expressions within each OR expression exclusively
    ## belong to one class, i.e. either Metrics or Dimensions
    if (
      all(
        sapply(
          X = object@.Data,
          FUN = function(gaOr) {
            length(
              unique(
                sapply(
                  X = gaOr,
                  FUN = class
                )
              )
            ) == 1
          }
        )
      )
    ) {
      return(TRUE)
    } else {
      return("An OR expression in a filter cannot mix metrics and dimensions.")
    }
  }
)

# ---- GA Dynamic and pre-defined segments ----

#' @export
setClass(
  Class = "gaDynSegment",
  contains = "gaAnd"
)

#' @export
setClass(
  Class = "gaSegmentId",
  contains = "character",
  validity = function(object) {
    pattern <- "^gaid::\\-?[0-9]+$"
    if (length(object) == 1) {
      if (grepl(pattern = pattern, x = object@.Data)) {
        return(TRUE)
      } else {
        paste("gaSegmentId must match the regular expression", pattern, sep = "")
      }
    } else {
      return("gaSegmentId must be a character vector of length 1")
    }
  }
)

setClassUnion(
  name = ".gaSegment",
  members = c("gaDynSegment", "gaSegmentId")
)

# setClassUnion(
#   name = ".gaLogical",
#   members = c(".gaOperator",".gaCompoundExpr")
# )

# ---- GA query dimensions, metrics, and sortby lists ----

#' @export
setClass(
  Class = "gaDateRange",
  representation = representation(
    startDate = "Date",
    endDate = "Date"
  ),
  prototype = prototype(
    startDate = Sys.Date() - 8,
    endDate = Sys.Date() - 1
  ),
  validity = function(object) {
    if (length(object@startDate) == length(object@endDate)) {
      if (all(object@endDate >= object@startDate)) {
        return(TRUE)
      } else {
        return("endDate cannot be before startDate")
      }
    } else {
      return("startDate and endDate must be the same length")
    }
  }
)

#' @export
setClass(
  Class = "gaMetrics",
  prototype = prototype(
    list(new("gaMetVar"))
  ),
  contains = "list",
  validity = function(object) {
    if (
      all(
        sapply(
          X = object,
          FUN = function(gaVar) {
            class(gaVar) == "gaMetVar"
          }
        )
      )
    ) {
      if (length(object) <= kGaMax$metrics) {
        return(TRUE)
      } else {
        paste("Maximum of", kGaMax$metrics, "metrics allowed.", sep = " ")
      }
    } else {
      return("Must be a list containing objects of class gaMetVar")
    }
  }
)

#' @export
setClass(
  Class = "gaDimensions",
  prototype = prototype(
    list(new("gaDimVar"))
  ),
  contains = "list",
  validity = function(object) {
    if (
      all(
        sapply(
          X = object,
          FUN = function(.gaVar) {
            class(.gaVar) == "gaDimVar"
          }
        )
      )
    ) {
      if (length(object) <= kGaMax$dimensions) {
        return(TRUE)
      } else {
        paste("Maximum of", kGaMax$dimensions, "dimensions allowed.", sep = " ")
      }
    } else {
      return("Must be a list containing objects of class gaDimVar")
    }
  }
)

#' @export
setClass(
  Class = "gaSortBy",
  representation = representation(
    desc = "logical"
  ),
  prototype = prototype(
    list(),
    desc = logical()
  ),
  contains = "list",
  validity = function(object) {
    if (
      all(
        sapply(
          X = object@.Data,
          FUN = function(gaVar) {
            inherits(gaVar, ".gaVar")
          }
        )
      )
    ) {
      if (length(object@.Data) == length(object@desc)) {
        return(TRUE)
      } else {
        return("List vector and desc vector must be of equal lengths")
      }
    } else {
      return("Must be a list containing objects of class .gaVar")
    }
  }
)

setClassUnion(
  name = ".gaVarList",
  members = c("gaMetrics", "gaDimensions", "gaSortBy"),
)

# ---- Ga Profile ID ----

#' @export
setClass(
  Class = "gaProfileId",
  contains = "character",
  validity = function(object) {
    if (
      all(
        sapply(
          X = object,
          FUN = function(profileId) {
            grepl(pattern = "^ga:[0-9]+$",  x = profileId)
          }
        )
      )
    ) {
      return(TRUE)
    } else {
      return("gaProfileId must be an string of digits preceeded by 'ga:'")
    }
  }
)

# -- GA query construct ----

#' @export
setClass(
  Class = "gaQuery",
  representation = representation(
    profileId = "gaProfileId",
    dateRange = "gaDateRange",
    metrics = "gaMetrics",
    dimensions = "gaDimensions",
    sortBy = "gaSortBy",
    filters = "gaFilter",
    segment = ".gaSegment",
    maxResults = "numeric",
    authFile = "character"
  ),
  prototype = prototype(
    dateRange = new("gaDateRange"),
    metrics = new("gaMetrics"),
    dimensions = new("gaDimensions"),
    sortBy = new("gaSortBy"),
    maxResults = kGaMaxResults
  ),
  validity = function(object) {
    if (length(object@maxResults) == 1) {
      if (object@maxResults >= 1) {
        if (object@maxResults <= kGaMaxRows) {
          if (!is.null(object@sortBy)) {
            if (
              all(
                !is.na(
                  match(
                    x = object@sortBy,
                    table = union(object@metrics, object@dimensions)
                  )
                )
              )
            ) {
              return(TRUE)
            } else {
              return("sortBy must contain varNames also used as metrics and/or dimensions")
            }
          } else {
            return(TRUE)
          }
        } else {
          return("maxResults cannot be greater than 1,000,000")
        }
      } else {
        return("maxResults must be at least 1")
      }
    } else {
      return("maxResults must be of length 1")
    }
  }
)

setClassUnion(
  name = ".gaUrlClasses",
  members = c(
    #".gaCompoundExpr",
    ".gaExpr", "gaOr", "gaAnd",
    
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