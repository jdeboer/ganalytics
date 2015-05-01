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

simpleCoerce <- function(from, to) {new(to, from)}
simpleCoerceData <- function(from, to) {new(to, from@.Data)}
simpleCoerceToNumeric <- function(from, to) {new(to, as.numeric(from))}
simpleCoerceToList <- function(from, to) {new(to, list(from))}
simpleReplace <- function(from, value) {initialize(from, value)}

# Coercing to .var subclasses
setAs(from = "character", to = "gaDimVar", def = simpleCoerce)
setAs(from = "character", to = "gaMetVar", def = simpleCoerce)
setAs(from = "character", to = "mcfDimVar", def = simpleCoerce)
setAs(from = "character", to = "mcfMetVar", def = simpleCoerce)
setAs(from = "character", to = "rtDimVar", def = simpleCoerce)
setAs(from = "character", to = "rtMetVar", def = simpleCoerce)

# Coercing to .operator subclasses
setAs(from = "character", to = "gaDimOperator", def = simpleCoerce)
setAs(from = "character", to = "gaMetOperator", def = simpleCoerce)
setAs(from = "character", to = "mcfDimOperator", def = simpleCoerce)
setAs(from = "character", to = "mcfMetOperator", def = simpleCoerce)
setAs(from = "character", to = "rtDimOperator", def = simpleCoerce)
setAs(from = "character", to = "rtMetOperator", def = simpleCoerce)

# Coercing to .operand subclasses
setAs(from = "character", to = "gaDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "gaMetOperand", def = simpleCoerce)
setAs(from = "character", to = "gaMetOperand", def = simpleCoerceToNumeric)
setAs(from = "character", to = "mcfDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "mcfMetOperand", def = simpleCoerce)
setAs(from = "character", to = "mcfMetOperand", def = simpleCoerceToNumeric)
setAs(from = "character", to = "rtDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "rtMetOperand", def = simpleCoerce)
setAs(from = "character", to = "rtMetOperand", def = simpleCoerceToNumeric)

# Coercing to character
setAs(from = ".metOperand", to = "character",
  def = function(from) {
    format(from@.Data, scientific = FALSE)
  },
  replace = function(from, value) {
    from@.Data <- as.numeric(value)
    validObject(from)
    from
  }
)

# Coercing GA expressions to GA API compatible character strings
setAs(from = ".expr", to = "character", def = function(from, to) {
  paste0(
    if(class(from) == "gaSegMetExpr") {
      if(from@metricScope != "") paste0(from@metricScope, "::")
    },
    as(from@var, to),
    as(from@operator, to),
    as(compileOperand(from), to)
  )
})

setAs(from = "andExpr", to = "character", def = function(from, to) {
  if(length(from) >= 1) {
    do.call(paste, c(lapply(from, as, to), sep = ";"))
  } else {
    character(0)
  }
})

setAs(from = "gaSequenceCondition", to = "character", def = function(from, to) {
  if(length(from) >= 1) {
    paste0(
      "sequence::",
      if(from@negation) {"!"} else {""},
      do.call(
        paste0,
        lapply(seq_along(from),
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
  } else {
    character(0)
  }
})

setAs(from = "gaNonSequenceCondition", to = "character", def = function(from) {
  paste0(
    "condition::",
    if(from@negation) {"!"} else {""},
    as(as(from, "andExpr"), "character")
  )
})

setAs(from = "gaSegmentCondition", to = "character", def = function(from, to) {
  if(length(from) >= 1) {
    paste(
      from@conditionScope,
      do.call(
        paste,
        c(
          lapply(from, FUN = function(dimOrMetCondition) {
              as(dimOrMetCondition, to)
          }),
          sep = ";"
        )
      ),
      sep = "::"
    )
  } else {
    character(0)
  }
})

setAs(from = "gaDynSegment", to = "character",
  def = function(from, to) {
    if(length(from) >= 1) {
      do.call(
        paste,
        c(
          lapply(from, FUN = function(segmentCondition) {
            as(segmentCondition, to)
          }),
          sep = ";"
        )
      )
    } else {
      character(0)
    }
  }
)

setAs(from = "gaSegmentId", to = "character",
  def = function(from) {
    as.character(from@.Data)
  },
  replace = function(from, value) {
    initialize(from, value)
  }
)

# Coercing from gaMetrics, gaDimensions, and gaSortBy to character
setAs(from = ".varList", to = "character",
  def = function(from) {
    paste(from@.Data, collapse = ",")
  },
  replace = function(from, value) {
    initialize(from, unlist(strsplit(value, split = ",")))
  }
)

setAs(from = ".sortBy", to = "character",
  def = function(from) {
    varNames <- sapply(from@.Data, FUN = function(varName) {
      as(varName, "character")
    })
    descChar <- ifelse(
      test = from@desc,
      yes = "-",
      no = ""
    )
    paste0(descChar, varNames, collapse = ",")
  },
  replace = function(from, value) {
    as(value, class(from))
  }
)

setAs(from = "orExpr", to = "character", def = function(from, to) {
  do.call(paste, c(lapply(from, as, to), sep = ","))
})

# Coercing to orExpr
setAs(from = ".expr", to = "orExpr", def = simpleCoerceToList)

setAs(from = "andExpr", to = "orExpr", def = function(from, to) {
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
    
    new(to, as.list(do.call(c, do.call(c, from@.Data))))
  }
)

# Coercing to andExpr
setAs(from = "orExpr", to = "andExpr", def = simpleCoerceToList)

setAs(from = ".expr", to = "andExpr", def = function(from, to) {
  new(to, list(
    as(from, "orExpr")
  ))
})

# Coercing to .gaExpr
setAs(from = "character", to = ".expr", def = function(from) {
  ops <- union(kGaOps$met, kGaOps$dim)
  ops <- str_replace_all(ops, "(\\[|\\])", "\\\\\\1")
  ops <- paste(ops, collapse = "|")
  operator <- str_match(from, ops)[1,1]
  x <- str_split_fixed(from, ops, 2)
  var <- Var(x[1,1])
  operand <- x[1,2]
  Expr(var, operator, parseOperand(operand, operator))
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

# Coercion to .filter subclasses

setAs(from = "andExpr", to = "gaFilter", def = simpleCoerce)

setAs(from = "orExpr", to = "gaFilter", def = function(from, to) {
  as(as(from, "andExpr"), to)
})

setAs(from = ".expr", to = "gaFilter", def = function(from, to) {
  as(as(from, "andExpr"), to)
})

setAs(from = "gaDynSegment", to = "gaFilter", def = simpleCoerceData)

# Coercing to gaDynSegment

setAs(from = "gaFilter", to = "gaDynSegment", def = simpleCoerceData)

setAs(from = "orExpr", to = "gaDynSegment", def = function(from, to) {
  as(as(from, "andExpr"), to)
})

setAs(from = "andExpr", to = "gaDynSegment", def = function(from, to) {
  new(to, list(GaSegmentCondition(GaNonSequenceCondition(from))))
})

setAs(from = ".expr", to = "gaDynSegment", def = function(from, to) {
  as(as(from, "andExpr"), to)
})

# Coercion to custom segment classes

setAs(from = ".compoundExpr", to = "gaSequenceCondition", def = function(from, to) {
  new(to, as("andExpr", from))
})

# Coercing to gaNonSequenceCondition
setAs(from = ".compoundExpr", to = "gaNonSequenceCondition", def = function(from, to) {
  new(to, as(from, "andExpr"))
})

# Coercing to gaSegmentCondition
setAs(from = ".compoundExpr", to = "gaSegmentCondition", def = function(from, to) {
  new(to, list(as("gaNonSequenceCondition", from)))
})

# Coercion to gaSegmentId

setAs(from = "character", to = "gaSegmentId", def = simpleCoerce)

setAs(from = "numeric", to = "gaSegmentId", def = function(from, to) {
  new(to, as.character(from))
})

setAs(from = "gaUserSegment", to = "gaSegmentId", def = function(from, to) {
  new(to, GaSegment(from))
})

# Coercion to numeric

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
    initialize(from, as.character(value))
  }
)

# Coercion to .sortBy subclasses

charToSortBy <- function(from, to, varFun) {
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
    FUN = varFun
  )
  new(to, varNames, desc = desc)
}

setAs(from = "character", to = "gaSortBy", def = function(from, to) {
  charToSortBy(from, to, GaVar)
})

setAs(from = "character", to = "mcfSortBy", def = function(from, to) {
  charToSortBy(from, to, McfVar)
})

setAs(from = "character", to = "rtSortBy", def = function(from, to) {
  charToSortBy(from, to, RtVar)
})

# Coercion to viewId
setAs(from = "numeric", to = "viewId", def = simpleCoerceData)
setAs(from = "character", to = "viewId", def = simpleCoerceData)

# Coercion to matrix
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
            startDate = StartDate(from),
            endDate = EndDate(from),
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
