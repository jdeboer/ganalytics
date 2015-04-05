#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
#' @include gtm-api-classes.R
#' @include management-api-classes.R
NULL

# Methods for coercion between classes
# ------------------------------------

# Coercing to and from gaDim or gaMet and character
setAs(
  from = "character",
  to = "gaMetVar",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = "character",
  to = "gaDimVar",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = "gaMetVar",
  to = "character",
  def = function(from) {
    return(from@.Data)
  },
  replace = function(from, value) {
    from <- initialize(from, value)
    return(from)
  }
)

setAs(
  from = "gaDimVar",
  to = "character",
  def = function(from) {
    return(from@.Data)
  },
  replace = function(from, value) {
    from <- initialize(from, value)
    return(from)
  }
)

# Coercing to and from gaDimOperator or gaMetOperator and character
setAs(
  from = "character",
  to = "gaDimOperator",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = "character",
  to = "gaMetOperator",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = "gaDimOperator",
  to = "character",
  def = function(from) {
    return(from@.Data)
  },
  replace = function(from, value) {
    from <- initialize(from, value)
    return(from)
  }
)

setAs(
  from = "gaMetOperator",
  to = "character",
  def = function(from) {
    return(from@.Data)
  },
  replace = function(from, value) {
    from <- initialize(from, value)
    return(from)
  }
)

# Coercing to and from gaDimOperand and character or gaMetOperand and numeric or character
setAs(
  from = "character",
  to = "gaDimOperand",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = "numeric",
  to = "gaMetOperand",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = "character",
  to = "gaMetOperand",
  def = function(from) {
    new(to, as.numeric(from))
  }
)

setAs(
  from = "gaMetOperand",
  to = "character",
  def = function(from) {
    format(from@.Data, scientific = FALSE)
  },
  replace = function(from, value) {
    from@.Data <- as.numeric(value)
    validObject(from)
    return(from)
  }
)

# Coercing to gaOr or gaAnd
setAs(
  from = ".gaExpr",
  to = "gaOr",
  def = function(from) {
    new(to, list(from))
  }
)

setAs(
  from = ".gaExpr",
  to = "gaAnd",
  def = function(from) {
    new(
      Class = to,
      list(
        as(
          object = from,
          Class = "gaOr"
        )
      )
    )
  }
)

setAs(
  from = "gaAnd",
  to = "gaOr",
  def = function(from) {
    # Break apart the AND expression into OR expressions
    # then break apart each OR expression into single
    # expressions. Concatenate the single expressions
    # back up the chain. Then convert array into a list of
    # expressions to use for a new AND expression.
    new(
      to, as.list(
        do.call(
          c, do.call(
            c, from@.Data
          )
        )
      )
    )
  }
)

setAs(
  from = "gaOr",
  to = "gaAnd",
  def = function(from) {
    new(to, list(from))
  }
)

# Coercing GA expressions into GA API compatible character strings
setAs(
  from = ".gaExpr",
  to = "character",
  def = function(from) {
    new(
      to,
      .Data <- paste(
        if(class(from) == "gaSegMetExpr") {
          if(from@metricScope != "") paste0(from@metricScope, "::")
        },
        as(from@gaVar, to),
        as(from@gaOperator, to),
        as(
          object = compileOperand(from),
          Class = to
        ),
        sep = ""
      )
    )
  }
)

compileOperand <- function(from) {
  operand <- gsub(
    pattern = "([,;])",
    replacement = "\\\\\\1",
    x = from@gaOperand
  )
  if (from@gaOperator == "[]") {
    operand <- paste0(operand, collapse = "|")
  } else if (from@gaOperator == "<>") {
    operand <- paste0(operand, collapse = "_")
  }
  operand
}

setAs(
  from = "gaOr",
  to = "character",
  def = function(from) {
    do.call(
      paste,
      c(
        lapply(
          X = from,
          FUN = function(gaExpr) {
            as(gaExpr, to)
          }
        ),
        sep = ","
      )
    )
  }
)

setAs(
  from = "gaAnd",
  to = "character",
  def = function(from) {
    if(length(from) >= 1) {
      do.call(
        paste,
        c(
          lapply(
            X = from,
            FUN = function(gaOr) {
              as(gaOr, to)
            }
          ),
          sep = ";"
        )
      )
    } else {
      character(0)
    }
  }
)

setAs(
  from = "gaSequenceCondition",
  to = "character",
  def = function(from) {
    if(length(from) >= 1) {
      paste0(
        "sequence::",
        if(from@negation) {"!"} else {""},
        do.call(
          paste0,
          c(
            lapply(
              X = seq_along(from),
              FUN = function(sequenceStep) {
                paste0(
                  if(sequenceStep > 1) {
                    if(from[[sequenceStep]]@immediatelyPrecedes) {";->"} else {";->>"}
                  } else {
                    if(from[[sequenceStep]]@immediatelyPrecedes) {"^"} else {""}
                  },
                  as(from[[sequenceStep]], to)
                )
              }
            )
          )
        )
      )
    } else {
      character(0)
    }
  }
)

setAs(
  from = "gaNonSequenceCondition",
  to = "character",
  def = function(from) {
    paste0(
      "condition::",
      if(from@negation) {"!"} else {""},
      as(as(from, "gaAnd"), "character")
    )
  }
)

setAs(
  from = "gaSegmentCondition",
  to = "character",
  def = function(from) {
    if(length(from) >= 1) {
      paste(
        from@conditionScope,
        do.call(
          paste,
          c(
            lapply(
              X = from,
              FUN = function(dimOrMetCondition) {
                as(dimOrMetCondition, to)
              }
            ),
            sep = ";"
          )
        ),
        sep = "::"
      )
    } else {
      character(0)
    }
  }
)

setAs(
  from = "gaDynSegment",
  to = "character",
  def = function(from) {
    if(length(from) >= 1) {
      do.call(
        paste,
        c(
          lapply(
            X = from,
            FUN = function(segmentCondition) {
              as(segmentCondition, to)
            }
          ),
          sep = ";"
        )
      )
    } else {
      character(0)
    }
  }
)

# Coercing to gaDynSegment and gaFilter
setAs(
  from = "gaOr",
  to = "gaDynSegment",
  def = function(from) {
    as(
      object = as(from, "gaAnd"),
      Class = to
    )
  }
)

setAs(
  from = "gaAnd",
  to = "gaDynSegment",
  def = function(from) {
    new(to, list(GaSegmentCondition(GaNonSequenceCondition(from))))
  }
)

setAs(
  from = ".gaExpr",
  to = "gaDynSegment",
  def = function(from) {
    as(
      object = as(from, "gaAnd"),
      Class = to
    )
  }
)

setAs(
  from = "gaOr",
  to = "gaFilter",
  def = function(from) {
    as(
      object = as(from, "gaAnd"),
      Class = to
    )
  }
)

setAs(
  from = "gaAnd",
  to = "gaFilter",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = ".gaExpr",
  to = "gaFilter",
  def = function(from) {
    as(
      object = as(from, "gaAnd"),
      Class = to
    )
  }
)

setAs(
  from = "gaDynSegment",
  to = "gaFilter",
  def = function(from) {
    new(Class=to, from@.Data)
  }
)

setAs(
  from = "gaFilter",
  to = "gaDynSegment",
  def = function(from) {
    new(Class=to, from@.Data)
  }
)

# Coercing to gaSegmentCondition and gaNonSequenceCondition
setAs(
  from = ".gaCompoundExpr",
  to = "gaSegmentCondition",
  def = function(from) {
    new(Class = to, list(GaNonSequenceCondition(from)))
  }
)

setAs(
  from = ".gaCompoundExpr",
  to = "gaNonSequenceCondition",
  def = function(from) {
    new(Class = to, GaAnd(from))
  }
)

# Coercing to gaSequenceCondition

setAs(
  from = ".gaCompoundExpr",
  to = "gaSequenceCondition",
  def = function(from) {
    new(Class = to, GaAnd(from))
  }
)

# Coercing to and from gaSegmentId
setAs(
  from = "numeric",
  to = "gaSegmentId",
  def = function(from) {
    new(to, as.character(from))
  }
)

setAs(
  from = "gaSegmentId",
  to = "numeric",
  def = function(from) {
    as.numeric(
      sub(
        pattern = "ga:([0-9]+)",
        replacement = "\\1",
        x = from@.Data
      )
    )
  },
  replace = function(from, value) {
    from <- initialize(from, as.character(value))
    return(from)
  }
)

setAs(
  from = "character",
  to = "gaSegmentId",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = "gaUserSegment",
  to = "gaSegmentId",
  def = function(from) {
    new(to, GaSegment(from))
  }
)

setAs(
  from = "gaSegmentId",
  to = "character",
  def = function(from) {
    as.character(from@.Data)
  },
  replace = function(from, value) {
    from <- initialize(from, value)
    return(from)
  }
)

# Coercing from gaMetrics, gaDimensions, and gaSortBy to character
setAs(
  from = ".gaVarList",
  to = "character",
  def = function(from) {
    paste(from@.Data, collapse = ",")
  },
  replace = function(from, value) {
    from <- initialize(
      .Object = from,
      unlist(
        strsplit(
          x = value,
          split = ","
        )
      )
    )
    return(from)
  }
)

setAs(
  from = "gaSortBy",
  to = "character",
  def = function(from) {
    varNames <- sapply(
      X = from@.Data,
      FUN = function(varName) {
        as(varName, "character")
      }
    )
    descChar <- ifelse(
      test = from@desc,
      yes = "-",
      no = ""
    )
    return(
      paste(descChar, varNames, sep = "", collapse = ",")
    )
  },
  replace = function(from, value) {
    from <- as(value, "gaSortBy")
  }
)

setAs(
  from = "character",
  to = "gaSortBy",
  def = function(from) {
    varNames <- unlist(
      strsplit(
        x = from@.Data,
        split = ","
      )
    )
    desc <- grepl(
      pattern = "^\\-",
      x = varNames
    )
    varNames <- sub(
      pattern = "^\\-",
      replacement = "",
      x = varNames
    )
    varNames <- lapply(
      X = varNames,
      FUN = GaVar
    )
    new(to, varNames, desc = desc)
  }
)

setAs(
  from = "numeric",
  to = "gaProfileId",
  def = function(from) {
    new("gaProfileId", from@.Data)
  }
)

setAs(
  from = "character",
  to = "gaProfileId",
  def = function(from) {
    new("gaProfileId", from@.Data)
  }
)

setAs(
  from = "gaQuery",
  to = "matrix",
  def = function(from) {
    profilesDatesSegments <- do.call(
      what = rbind,
      args = lapply(
        X = GaProfileId(from),
        FUN = function(profileId) {
          data.frame(
            startDate = GaStartDate(from),
            endDate = GaEndDate(from),
            profileId = profileId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(startDate, endDate, profileId) {
        c(
          "ids" = as(GaProfileId(profileId), "character"),
          "start-date" = as.character(startDate),
          "end-date" = as.character(endDate),
          "metrics" = as(GaMetrics(from), "character"),
          "dimensions" = if(length(GaDimensions(from)) >= 1) {
            as(GaDimensions(from), "character")
          },
          "sort" = if(length(GaSortBy(from)) >= 1) {
            as(GaSortBy(from), "character")
          },
          "filters" = if(length(GaFilter(from)) >= 1) {
            as(GaFilter(from), "character")
          },
          "segment" = if(length(GaSegment(from)) >= 1) {
            as(GaSegment(from), "character")
          },
          "samplingLevel" = as(GaSamplingLevel(from), "character")
        )
      },
      profilesDatesSegments$startDate,
      profilesDatesSegments$endDate,
      profilesDatesSegments$profileId
    )
  }
)
