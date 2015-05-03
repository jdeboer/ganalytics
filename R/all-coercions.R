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
coerceViaList <- function(from, to) {as(as.list(from), to)}
simpleReplace <- function(from, value) {initialize(from, value)}
coerceLogicalOperand <- function(from, to){
  operand <- ifelse(from, yes = "Yes", no = "No")
  if (is.na(operand)) operand <- from
  new(to, operand)
}

# Coercing to .var classes

setAs(from = "character", to = ".gaVar", def = function(from) {
  tryCatch(
    as(from, "gaMetVar"),
    error = function(e1) {
      tryCatch(
        as(from, "gaDimVar"),
        error = function(e2) {
          stop(e1, e2)
        }
      )
    }
  )
})

setAs(from = "character", to = ".mcfVar", def = function(from) {
  tryCatch(
    as(from, "mcfMetVar"),
    error = function(e1) {
      tryCatch(
        as(from, "mcfDimVar"),
        error = function(e2) {
          stop(e1, e2)
        }
      )
    }
  )
})

setAs(from = "character", to = ".rtVar", def = function(from) {
  tryCatch(
    as(from, "rtMetVar"),
    error = function(e1) {
      tryCatch(
        as(from, "rtDimVar"),
        error = function(e2) {
          stop(e1, e2)
        }
      )
    }
  )
})

setAs(from = "character", to = ".metVar", def = function(from) {
  tryCatch(
    as(from, "gaMetVar"),
    error = function(e1) {
      tryCatch(
        as(from, "mcfMetVar"),
        error = function(e2) {
          tryCatch(
            as(from, "rtMetVar"),
            error = function(e3) {
              stop(e1, e2, e3)
            }
          )
        }
      )
    }
  )
})

setAs(from = "character", to = ".dimVar", def = function(from) {
  tryCatch(
    as(from, "gaDimVar"),
    error = function(e1) {
      tryCatch(
        as(from, "mcfDimVar"),
        error = function(e2) {
          tryCatch(
            as(from, "rtDimVar"),
            error = function(e3) {
              stop(e1, e2, e3)
            }
          )
        }
      )
    }
  )
})

setAs(from = "character", to = ".var", def = function(from) {
  tryCatch(
    as(from, ".gaVar"),
    error = function(e1) {
      tryCatch(
        as(from, ".mcfVar"),
        error = function(e2) {
          tryCatch(
            as(from, ".rtVar"),
            error = function(e3) {
              stop(e1, e2, e3)
            }
          )
        }
      )
    }
  )
})

setAs(from = "character", to = "gaDimVar", def = simpleCoerce)
setAs(from = "character", to = "gaMetVar", def = simpleCoerce)
setAs(from = "character", to = "mcfDimVar", def = simpleCoerce)
setAs(from = "character", to = "mcfMetVar", def = simpleCoerce)
setAs(from = "character", to = "rtDimVar", def = simpleCoerce)
setAs(from = "character", to = "rtMetVar", def = simpleCoerce)

setAs(from = ".mcfVar", to = ".gaVar", def = function(from, to) {
  as(sub("mcf:", "ga:", from), to)
})

setAs(from = ".rtVar", to = ".gaVar", def = function(from, to) {
  as(sub("rt:", "ga:", from), to)
})

setAs(from = ".gaVar", to = ".mcfVar", def = function(from, to) {
  as(sub("ga:", "mcf:", from), to)
})

setAs(from = ".rtVar", to = ".mcfVar", def = function(from, to) {
  as(sub("rt:", "mcf:", from), to)
})

setAs(from = ".gaVar", to = ".rtVar", def = function(from, to) {
  as(sub("ga:", "rt:", from), to)
})

setAs(from = ".mcfVar", to = ".rtVar", def = function(from, to) {
  as(sub("mcf:", "rt:", from), to)
})

setAs(from = ".expr", to = ".var",
  def = function(from, to) {
    from@var
  },
  replace = function(from, value) {
    from@var <- value
    validObject(from)
    from
  })

# Coercing to .operator subclasses
setAs(from = "character", to = "gaDimOperator", def = simpleCoerce)
setAs(from = "character", to = "gaMetOperator", def = simpleCoerce)
setAs(from = "character", to = "mcfDimOperator", def = simpleCoerce)
setAs(from = "character", to = "mcfMetOperator", def = simpleCoerce)
setAs(from = "character", to = "rtDimOperator", def = simpleCoerce)
setAs(from = "character", to = "rtMetOperator", def = simpleCoerce)

setAs(from = ".expr", to = ".operator",
      def = function(from, to) {
        from@operator
      },
      replace = function(from, value) {
        from@operator <- value
        validObject(from)
        from
      })

# Coercing to .operand subclasses
setAs(from = "character", to = "gaDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "gaDimOperand", def = function(from, to){
  as(as(from, "character"), to)
})
setAs(from = "logical", to = "gaDimOperand", def = coerceLogicalOperand)
setAs(from = "numeric", to = "gaMetOperand", def = simpleCoerce)
setAs(from = "character", to = "gaMetOperand", def = simpleCoerceToNumeric)
setAs(from = "character", to = "mcfDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "mcfDimOperand", def = function(from, to){
  as(as(from, "character"), to)
})
setAs(from = "logical", to = "mcfDimOperand", def = coerceLogicalOperand)
setAs(from = "numeric", to = "mcfMetOperand", def = simpleCoerce)
setAs(from = "character", to = "mcfMetOperand", def = simpleCoerceToNumeric)
setAs(from = "character", to = "rtDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "rtDimOperand", def = function(from, to){
  as(as(from, "character"), to)
})
setAs(from = "logical", to = "rtDimOperand", def = coerceLogicalOperand)
setAs(from = "numeric", to = "rtMetOperand", def = simpleCoerce)
setAs(from = "character", to = "rtMetOperand", def = simpleCoerceToNumeric)

setAs(from = ".expr", to = ".operand",
  def = function(from, to) {
    from@operand
  },
  replace = function(from, value) {
    from@operand <- as(value, ".operand")
    validObject(from)
    from
  })


compileOperand <- function(from) {
  unEscapedOperand <- as(as(from, ".operand"), "character")
  operator <- as(as(from, ".operator"), "character")
  compiledOperand <- gsub(
    pattern = "(,|;|\\\\)", # What about _ and | used within an operand when using <> or [] operators
    replacement = "\\\\\\1",
    x = unEscapedOperand
  )
  if (operator == "[]") {
    compiledOperand <- paste0(compiledOperand, collapse = "|")
  } else if (from@operator == "<>") {
    compiledOperand <- paste0(compiledOperand, collapse = "_")
  }
  compiledOperand
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

# Coercing to logical
setAs(from = ".dimOperand", to = "logical",
  def = function(from, to) {
    YesNo <- c("Yes" = TRUE, "No" = FALSE)
    index <- pmatch(tolower(from), tolower(names(YesNo)))
    YesNo[index]
  }
)

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

setAs(from = "orExpr", to = "character", def = function(from, to) {
  do.call(paste, c(lapply(from, as, to), sep = ","))
})

setAs(from = "andExpr", to = "character", def = function(from, to) {
  if(length(from) >= 1) {
    do.call(paste, c(lapply(from, as, to), sep = ";"))
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
    descChar <- ifelse(from@desc, "-", "")
    paste0(descChar, varNames, collapse = ",")
  },
  replace = function(from, value) {
    as(value, class(from))
  }
)

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

# Coercion to .filter subclasses

setAs(from = "andExpr", to = ".tableFilter", def = function(from) {
  if(all_inherit(unlist(from), ".gaExpr")) {
    as(from, "gaFilter")
  } else if(all_inherit(unlist(from), ".mcfExpr")) {
    as(from, "mcfFilter")
  } else if(all_inherit(unlist(from), ".rtExpr")) {
    as(from, "rtFilter")
  } else stop("Cannot determine type of filter.")
})

setAs(from = "NULL", to = "gaFilter", def = coerceViaList)
setAs(from = "NULL", to = "mcfFilter", def = coerceViaList)
setAs(from = "NULL", to = "rtFilter", def = coerceViaList)

setAs(from = "andExpr", to = "gaFilter", def = simpleCoerce)
setAs(from = "andExpr", to = "mcfFilter", def = simpleCoerce)
setAs(from = "andExpr", to = "rtFilter", def = simpleCoerce)

setAs(from = "orExpr", to = "gaFilter", def = function(from, to) {
  as(as(from, "andExpr"), to)
})

setAs(from = "orExpr", to = "gaFilter", def = coerceToAndFirst)
setAs(from = "orExpr", to = "mcfFilter", def = coerceToAndFirst)
setAs(from = "orExpr", to = "rtFilter", def = coerceToAndFirst)
setAs(from = "orExpr", to = ".tableFilter", def = coerceToAndFirst)
setAs(from = ".expr", to = "gaFilter", def = coerceToAndFirst)
setAs(from = ".expr", to = "mcfFilter", def = coerceToAndFirst)
setAs(from = ".expr", to = "rtFilter", def = coerceToAndFirst)
setAs(from = ".expr", to = ".tableFilter", def = coerceToAndFirst)

setAs(from = "gaDynSegment", to = "gaFilter", def = simpleCoerceData)
setAs(from = "gaDynSegment", to = ".tableFilter", def = simpleCoerceData)

setAs(from = ".query", to = ".tableFilter",
  def = function(from, to){
    from@filters
  },
  replace = function(from, value) {
    from@filters <- as(value, ".tableFilter")
    validObject(from)
    from
  }
)

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

setAs(from = "gaQuery", to = ".gaSegment",
  def = function(from) {
    from@segment
  },
  replace = function(from, value) {
    from@segment <- as(value, ".gaSegment") # Need to define coercions to .gaSegment from char and numeric
    validObject(from)
    from
  }
)
setAs(from = ".query", to = ".dimensions",
  def = function(from, to) {
    from@dimensions
  },
  replace = function(from, value) {
    from@dimensions <- as(value, ".dimensions")
    validObject(from)
    from
  }
)

setAs(from = ".query", to = ".metrics",
      def = function(from, to) {
        from@metrics
      },
      replace = function(from, value) {
        from@metrics <- as(value, ".metrics")
        validObject(from)
        from
      }
)

setAs(from = ".query", to = ".sortBy",
      def = function(from, to) {
        from@sortBy
      },
      replace = function(from, value) {
        from@sortBy <- as(value, ".sortBy")
        validObject(from)
        from
      }
)

setAs(from = "list", to = ".gaVarList", def = function(from) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".gaVar")}))
  if (all_inherit(vars, ".metVar")) {
    as(vars, "gaMetrics")
  } else if(all_inherit(vars, ".dimVar")) {
    as(vars, "gaDimensions")
  } else {
    as(vars, "gaSortBy")
  }
})

setAs(from = "list", to = ".mcfVarList", def = function(from) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".mcfVar")}))
  if (all_inherit(vars, ".metVar")) {
    as(vars, "mcfMetrics")
  } else if(all_inherit(vars, ".dimVar")) {
    as(vars, "mcfDimensions")
  } else {
    as(vars, "mcfSortBy")
  }
})

setAs(from = "list", to = ".rtVarList", def = function(from) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".rtVar")}))
  if (all_inherit(vars, ".metVar")) {
    as(vars, "rtMetrics")
  } else if(all_inherit(vars, ".dimVar")) {
    as(vars, "rtDimensions")
  } else {
    as(vars, "rtSortBy")
  }
})

setAs(from = "list", to = "gaDimensions", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), "gaDimVar")}))
  new(to, vars)
})

setAs(from = "list", to = "gaMetrics", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), "gaMetVar")}))
  new(to, vars)
})

setAs(from = "list", to = "gaSortBy", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".gaVar")}))
  new(to, vars)
})

setAs(from = "list", to = "mcfDimensions", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), "mcfDimVar")}))
  new(to, vars)
})

setAs(from = "list", to = "mcfMetrics", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), "mcfMetVar")}))
  new(to, vars)
})

setAs(from = "list", to = "mcfSortBy", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".mcfVar")}))
  new(to, vars)
})

setAs(from = "list", to = "rtDimensions", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), "rtDimVar")}))
  new(to, vars)
})

setAs(from = "list", to = "rtMetrics", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), "rtMetVar")}))
  new(to, vars)
})

setAs(from = "list", to = "rtSortBy", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".rtVar")}))
  new(to, vars)
})

setAs(from = "character", to = "gaDimensions", def = coerceViaList)
setAs(from = "character", to = "gaMetrics", def = coerceViaList)
setAs(from = "character", to = "gaSortBy", def = coerceViaList)
setAs(from = "character", to = "mcfDimensions", def = coerceViaList)
setAs(from = "character", to = "mcfMetrics", def = coerceViaList)
setAs(from = "character", to = "mcfSortBy", def = coerceViaList)
setAs(from = "character", to = "rtDimensions", def = coerceViaList)
setAs(from = "character", to = "rtMetrics", def = coerceViaList)
setAs(from = "character", to = "rtSortBy", def = coerceViaList)

setAs(from = "NULL", to = "gaDimensions", def = coerceViaList)
setAs(from = "NULL", to = "gaMetrics", def = coerceViaList)
setAs(from = "NULL", to = "gaSortBy", def = coerceViaList)
setAs(from = "NULL", to = "mcfDimensions", def = coerceViaList)
setAs(from = "NULL", to = "mcfMetrics", def = coerceViaList)
setAs(from = "NULL", to = "mcfSortBy", def = coerceViaList)
setAs(from = "NULL", to = "rtDimensions", def = coerceViaList)
setAs(from = "NULL", to = "rtMetrics", def = coerceViaList)
setAs(from = "NULL", to = "rtSortBy", def = coerceViaList)

# Coercion to .sortBy subclasses

setAs(from = "character", to = ".sortBy", def = function(from) {
  varChars <- unlist(strsplit(from, ","))
  vars <- lapply(varChars, function(x) {
    as(sub("^(\\+|\\-)","",x), ".var")
  })
  vars <- as(vars, ".sortBy")
  desc <- logical()
  desc[grep("^\\+", varChars)] <- FALSE
  desc[grep("^\\-", varChars)] <- TRUE
  vars@desc <- desc
  vars
})

setAs(from = "character", to = "gaSortBy", def = function(from) {
  as(from, ".sortBy")
})

setAs(from = "character", to = "mcfSortBy", def = function(from, to) {
  as(from, ".sortBy")
})

setAs(from = "character", to = "rtSortBy", def = function(from, to) {
  as(from, ".sortBy")
})

# Coercion to viewId
setAs(from = "numeric", to = "viewId", def = simpleCoerceData)
setAs(from = "character", to = "viewId", def = simpleCoerceData)
setAs(from = ".query", to = "viewId",
  def = function(from, to) {
    from@viewId
  },
  replace = function(from, value) {
    from@viewId <- as(value, "viewId")
    validObject(from)
    from
  }
)

setAs(from = "gaView", to = "viewId",
  def = function(from, to) {
    as(from$id, "viewId")
  },
  replace = function(from, value) {
    from$id <- as(value, "viewId")
  }
)

# Select the default view of the property
setAs(from = "gaProperty", to = "viewId",
  def = function(from, to) {
    as(from$defaultView, "viewId")
  },
  replace = function(from, value) {
    as(from$defaultView, "viewId") <- as(value, "viewId")
  }
)

# Sselect the first property of the account, which is then
# used to select a view (as above).
setAs(from = "gaAccount", to = "viewId",
  def = function(from, to) {
    as(from$properties$entities[[1]], "viewId")
  },
  replace = function(from, value) {
    as(from$properties$entities[[1]], "viewId") <- as(value, "viewId")
  }
)

# Coercion to dateRange
setAs(from = ".query", to = "dateRange",
  def = function(from, to) {
    from@dateRange
  },
  replace = function(from, value) {
    from@dateRange <- as(value, "dateRange")
    validObject(from)
    from
  }
)

# Coercion to matrix
setAs(
  from = "gaQuery",
  to = "matrix",
  def = function(from) {
    views <- as(from, "viewId")
    dateRange <- as(from, "dateRange")
    startDates <- dateRange@startDate
    endDates <- dateRange@endDate
    viewsDatesSegments <- do.call(
      what = rbind,
      args = lapply(
        X = views,
        FUN = function(viewId) {
          data.frame(
            startDate = startDates,
            endDate = endDates,
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(startDate, endDate, viewId) {
        metrics <- as(from, ".metrics")
        dimensions <- as(from, ".dimensions")
        sortBy <- as(from, ".sortBy")
        tableFilter <- as(from, ".tableFilter")
        segment <- as(from, ".gaSegment")
        c(
          "ids" = as(viewId, "character"),
          "start-date" = as.character(startDate),
          "end-date" = as.character(endDate),
          "metrics" = as(metrics, "character"),
          "dimensions" = if(length(dimensions) >= 1) {
            as(dimensions, "character")
          },
          "sort" = if(length(sortBy) >= 1) {
            as(sortBy, "character")
          },
          "filters" = if(length(tableFilter) >= 1) {
            as(tableFilter, "character")
          },
          "segment" = if(length(segment) >= 1) {
            as(segment, "character")
          },
          "samplingLevel" = as(from@samplingLevel, "character")
        )
      },
      viewsDatesSegments$startDate,
      viewsDatesSegments$endDate,
      viewsDatesSegments$viewId
    )
  }
)

setAs(
  from = "mcfQuery",
  to = "matrix",
  def = function(from) {
    views <- as(from, "viewId")
    dateRange <- as(from, "dateRange")
    startDates <- dateRange@startDate
    endDates <- dateRange@endDate
    viewsDates <- do.call(
      what = rbind,
      args = lapply(
        X = views,
        FUN = function(viewId) {
          data.frame(
            startDate = startDates,
            endDate = endDates,
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(startDate, endDate, viewId) {
        metrics <- as(from, ".metrics")
        dimensions <- as(from, ".dimensions")
        sortBy <- as(from, ".sortBy")
        tableFilter <- as(from, ".tableFilter")
        c(
          "ids" = as(viewId, "character"),
          "start-date" = as.character(startDate),
          "end-date" = as.character(endDate),
          "metrics" = as(metrics, "character"),
          "dimensions" = if(length(dimensions) >= 1) {
            as(dimensions, "character")
          },
          "sort" = if(length(sortBy) >= 1) {
            as(sortBy, "character")
          },
          "filters" = if(length(tableFilter) >= 1) {
            as(tableFilter, "character")
          },
          "samplingLevel" = as(from@samplingLevel, "character")
        )
      },
      viewsDates$startDate,
      viewsDates$endDate,
      viewsDates$viewId
    )
  }
)
