#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
#' @include gtm-api-classes.R
#' @include management-api-classes.R
#' @importFrom assertthat assert_that
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
  from = ".var",
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
  from = ".operator",
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
  from = ".metOperand",
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
  from = ".expr",
  to = "orExpr",
  def = function(from) {
    new(to, list(from))
  }
)

setAs(
  from = ".expr",
  to = "andExpr",
  def = function(from) {
    new(Class = to, list(as(object = from, Class = "orExpr")))
  }
)

setAs(
  from = "andExpr",
  to = "orExpr",
  def = function(from) {
    # This is currently only legal if the gaAnd object does not contain any gaOr
    # object of length greater than 1 OR if there is only one gaOr. Otherwise,
    # in a future implementation if any gaOr objects have a length greater than
    # 1, then they will need to be shortened to length 1 which is only possible
    # if each expression within that gaOr shares the same dimension and the
    # expression operators and operands can be combined either as a match regex
    # or a match list.
    
    # Check that all contained gaOr objects in the list have a length of 1
    assert_that(all(sapply(from, length) == 1) | length(from) == 1)
    
    # Break apart the AND expression into OR expressions
    # then break apart each OR expression into single
    # expressions. Concatenate the single expressions
    # back up the chain. Then convert array into a list of
    # expressions to use for a new OR expression.
    
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
  from = "orExpr",
  to = "andExpr",
  def = function(from) {
    new(to, list(from))
  }
)

# Coercing GA expressions to and from GA API compatible character strings
setAs(
  from = ".expr",
  to = "character",
  def = function(from) {
    new(
      to,
      .Data <- paste(
        if(class(from) == "gaSegMetExpr") {
          if(from@metricScope != "") paste0(from@metricScope, "::")
        },
        as(from@var, to),
        as(from@operator, to),
        as(
          object = compileOperand(from),
          Class = to
        ),
        sep = ""
      )
    )
  }
)

setAs(from = "character", to = ".gaExpr", def = function(from) {
  ops <- union(kGaOps$met, kGaOps$dim)
  ops <- str_replace_all(ops, "(\\[|\\])", "\\\\\\1")
  ops <- paste(ops, collapse = "|")
  operator <- str_match(from, ops)[1,1]
  x <- str_split_fixed(from, ops, 2)
  var <- x[1,1]
  operand <- x[1,2]
  expr <- GaExpr(var, operator, parseOperand(operand, operator))
  expr
})

compileOperand <- function(from) {
  operand <- gsub(
    pattern = "(,|;|\\\\)", # What about _ and | used within an operand when using <> or [] operators
    replacement = "\\\\\\1",
    x = from@operand
  )
  if (from@operator == "[]") {
    operand <- paste0(operand, collapse = "|")
  } else if (from@operator == "<>") {
    operand <- paste0(operand, collapse = "_")
  }
  operand
}

# Need to consider escaping of the following characters in the operand:\|,;_
parseOperand <- function(operand, operator) {
  if (operator == "[]") {
    operand <- str_split(operand, "\\|")[[1]]
  } else if (operator == "<>") {
    operand <- str_split_fixed(operand, "_", 2)[1,]
  }
  operand <- gsub("\\\\", "\\", operand)
}

setAs(
  from = "orExpr",
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
  from = "andExpr",
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
      as(as(from, "andExpr"), "character")
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
  from = "orExpr",
  to = "gaDynSegment",
  def = function(from) {
    as(
      object = as(from, "andExpr"),
      Class = to
    )
  }
)

setAs(
  from = "andExpr",
  to = "gaDynSegment",
  def = function(from) {
    new(to, list(GaSegmentCondition(GaNonSequenceCondition(from))))
  }
)

setAs(
  from = ".expr",
  to = "gaDynSegment",
  def = function(from) {
    as(
      object = as(from, "andExpr"),
      Class = to
    )
  }
)

setAs(
  from = "orExpr",
  to = "gaFilter",
  def = function(from) {
    as(
      object = as(from, "andExpr"),
      Class = to
    )
  }
)

setAs(
  from = "andExpr",
  to = "gaFilter",
  def = function(from) {
    new(to, from)
  }
)

setAs(
  from = ".expr",
  to = "gaFilter",
  def = function(from) {
    as(
      object = as(from, "andExpr"),
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
  from = ".compoundExpr",
  to = "gaSegmentCondition",
  def = function(from) {
    new(Class = to, list(GaNonSequenceCondition(from)))
  }
)

setAs(
  from = ".compoundExpr",
  to = "gaNonSequenceCondition",
  def = function(from) {
    new(Class = to, GaAnd(from))
  }
)

# Coercing to gaSequenceCondition

setAs(
  from = ".compoundExpr",
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
  to = "viewId",
  def = function(from) {
    new("viewId", from@.Data)
  }
)

setAs(
  from = "character",
  to = "viewId",
  def = function(from) {
    new("viewId", from@.Data)
  }
)

setAs(
  from = "gaQuery",
  to = "matrix",
  def = function(from) {
    viewsDatesSegments <- do.call(
      what = rbind,
      args = lapply(
        X = GaView(from),
        FUN = function(viewId) {
          data.frame(
            startDate = GaStartDate(from),
            endDate = GaEndDate(from),
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(startDate, endDate, viewId) {
        c(
          "ids" = as(GaView(viewId), "character"),
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
      viewsDatesSegments$startDate,
      viewsDatesSegments$endDate,
      viewsDatesSegments$viewId
    )
  }
)
