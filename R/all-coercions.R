#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include utils.R
#' @include gtm-api-classes.R
#' @include management-api-classes.R
#' @importFrom assertthat assert_that
#' @importFrom stringr str_split str_split_fixed
NULL

# Methods for coercion between classes
# ------------------------------------

simpleCoerce <- function(from, to) {new(to, from)}
simpleCoerceData <- function(from, to) {new(to, from@.Data)}
simpleCoerceToNumeric <- function(from, to) {new(to, as.numeric(from))}
simpleCoerceToList <- function(from, to) {new(to, list(from))}
coerceViaList <- function(from, to) {as(as.list(from), to)}
coerceViaChar <- function(from, to){as(as(from, "character"), to)}
coerceViaAnd <- function(from, to) {as(as(from, "andExpr"), to)}
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

coerceVarNS <- function(from, to) {
  fromClass <- class(from)
  toClass <- to
  if(extends(fromClass, ".gaVar")) fromNS <- "ga"
  if(extends(toClass, ".gaVar")) toNS <- "ga"
  if(extends(fromClass, ".mcfVar")) fromNS <- "mcf"
  if(extends(toClass, ".mcfVar")) toNS <- "mcf"
  if(extends(fromClass, ".rtVar")) fromNS <- "rt"
  if(extends(toClass, ".rtVar")) toNS <- "rt"
  fromNS <- paste0(fromNS, ":")
  toNS <- paste0(toNS, ":")
  as(as(sub(fromNS, toNS, from), "character"), to)
}

setAs(from = ".mcfVar", to = ".gaVar", def = coerceVarNS)
setAs(from = ".rtVar", to = ".gaVar", def = coerceVarNS)
setAs(from = ".gaVar", to = ".mcfVar", def = coerceVarNS)
setAs(from = ".rtVar", to = ".mcfVar", def = coerceVarNS)
setAs(from = ".gaVar", to = ".rtVar", def = coerceVarNS)
setAs(from = ".mcfVar", to = ".rtVar", def = coerceVarNS)

setAs(from = ".expr", to = ".var",
  def = function(from, to) {
    from@var
  },
  replace = function(from, value) {
    Expr(value, from@comparator, from@operand)
  }
)

setAs(from = ".expr", to = ".gaVar",
  def = function(from, to) {
    as(as(from, ".var"), to)
  },
  replace = function(from, value) {
    as(from, ".var") <- as(value, ".gaVar")
    from
  }
)

# Coercing to .comparator subclasses
setAs(from = "character", to = "gaDimComparator", def = simpleCoerce)
setAs(from = "character", to = "gaMetComparator", def = simpleCoerce)
setAs(from = "character", to = "mcfDimComparator", def = simpleCoerce)
setAs(from = "character", to = "mcfMetComparator", def = simpleCoerce)
setAs(from = "character", to = "rtDimComparator", def = simpleCoerce)
setAs(from = "character", to = "rtMetComparator", def = simpleCoerce)

#############\/ Transform to method of Comparator and Comparator<- generic functions

setAs(from = ".expr", to = ".comparator",
  def = function(from, to) {
    from@comparator
  },
  replace = function(from, value) {
    use_class <- class(from@comparator)
    from@comparator <- as(value, use_class)
    validObject(from)
    from
  })

# Coercing to .operand subclasses
setAs(from = "character", to = "gaDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "gaDimOperand", def = coerceViaChar)
setAs(from = "logical", to = "gaDimOperand", def = coerceLogicalOperand)
setAs(from = "numeric", to = "gaMetOperand", def = simpleCoerce)
setAs(from = "character", to = "gaMetOperand", def = simpleCoerceToNumeric)
setAs(from = "character", to = "mcfDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "mcfDimOperand", def = coerceViaChar)
setAs(from = "logical", to = "mcfDimOperand", def = coerceLogicalOperand)
setAs(from = "numeric", to = "mcfMetOperand", def = simpleCoerce)
setAs(from = "character", to = "mcfMetOperand", def = simpleCoerceToNumeric)
setAs(from = "character", to = "rtDimOperand", def = simpleCoerce)
setAs(from = "numeric", to = "rtDimOperand", def = coerceViaChar)
setAs(from = "logical", to = "rtDimOperand", def = coerceLogicalOperand)
setAs(from = "numeric", to = "rtMetOperand", def = simpleCoerce)
setAs(from = "character", to = "rtMetOperand", def = simpleCoerceToNumeric)

setAs(from = "character", to = ".dimOperand", def = simpleCoerce)
setAs(from = "numeric", to = ".metOperand", def = simpleCoerce)

setAs(from = "character", to = ".operand", def = function(from){
  as(from, ".dimOperand")
})

setAs(from = "numeric", to = ".operand", def = function(from){
  as(from, ".metOperand")
})

#############\/ Transform to method of Operand and Operand<- generic functions

setAs(from = ".expr", to = ".operand",
  def = function(from, to) {
    from@operand
  },
  replace = function(from, value) {
    use_class <- class(from@operand)
    from@operand <- as(value, use_class)
    validObject(from)
    from
  })

## As setAs looks within the global namespace for any internall called functions within
# the definition, then the following function is likely to throw an error if used with setAs.
compileOperand <- function(from) {
  unEscapedOperand <- as(as(from, ".operand"), "character")
  comparator <- as(as(from, ".comparator"), "character")
  compiledOperand <- gsub(
    pattern = "(,|;|\\\\)", # What about _ and | used within an operand when using <> or [] comparators
    replacement = "\\\\\\1",
    x = unEscapedOperand
  )
  if (comparator == "[]") {
    compiledOperand <- paste0(compiledOperand, collapse = "|")
  } else if (from@comparator == "<>") {
    compiledOperand <- paste0(compiledOperand, collapse = "_")
  }
  compiledOperand
}

# Need to consider escaping of the following characters in the operand:\|,;_
parseOperand <- function(operand, comparator) {
  if (comparator == "[]") {
    operand <- str_split(operand, "\\|")[[1]]
  } else if (comparator == "<>") {
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

## the folowing function calls another function which is not in the global namesapce
# Coercing GA expressions to GA API compatible character strings
setAs(from = ".expr", to = "character", def = function(from, to) {
  paste0(
    if(class(from) == "gaSegMetExpr") {
      if(from@metricScope != "") paste0(from@metricScope, "::")
    },
    as(from@var, to),
    as(from@comparator, to),
    as(compileOperand(from), to)
  )
})

setAs(from = "orExpr", to = "character", def = function(from, to) {
  do.call(paste, c(lapply(from, as, to), sep = ","))
})

setAs(from = "andExpr", to = "character", def = function(from, to) {
  do.call(paste, c(lapply(from, as, to), sep = ";"))
})

setAs(from = "gaSegmentConditionFilter", to = "character", def = function(from) {
  paste0(
    "condition::",
    if(from@negation) {"!"} else {""},
    as(as(from, "andExpr"), "character")
  )
})

setAs(from = "gaSegmentSequenceFilter", to = "character", def = function(from, to) {
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

setAs(from = "gaSegmentFilterList", to = "character", def = function(from, to) {
  if(length(from) >= 1) {
    paste(
      from@conditionScope,
      do.call(
        paste,
        c(
          lapply(from, FUN = function(compoundExpr) {
              as(compoundExpr, to)
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
    do.call(
      paste,
      c(
        lapply(from, FUN = function(segmentCondition) {
          as(segmentCondition, to)
        }),
        sep = ";"
      )
    )
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

# Coercing from .metrics, .dimensions, and .sortBy to character
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

# Coercing to .expr
# Need to redo this to properly handle escaping with the \ used with GA.
setAs(from = "character", to = ".expr", def = function(from) {
  ops <- union(kGaOps$met, kGaOps$dim)
  ops <- str_replace_all(ops, "(\\[|\\])", "\\\\\\1")
  ops <- paste(ops, collapse = "|")
  comparator <- str_match(from, ops)[1,1]
  x <- str_split_fixed(from, ops, 2)
  var <- Var(x[1,1])
  operand <- x[1,2]
  Expr(var, comparator, parseOperand(operand, comparator))
})

# Coercing to orExpr
setAs(from = ".expr", to = "orExpr", def = simpleCoerceToList)

setAs(from = "andExpr", to = "orExpr", def = function(from, to) {
    # This is currently only legal if the gaAnd object does not contain any gaOr
    # object of length greater than 1 OR if there is only one gaOr. Otherwise,
    # in a future implementation if any gaOr objects have a length greater than
    # 1, then they will need to be shortened to length 1 which is only possible
    # if each expression within that gaOr shares the same dimension and the
    # expression comparators and operands can be combined either as a match regex
    # or a match list.

    # Check that all contained gaOr objects in the list have a length of 1
    assert_that(all(sapply(from, length) == 1) | length(from) == 1)

    # Break apart the AND expression into OR expressions
    # then break apart each OR expression into single
    # expressions. Concatenate the single expressions
    # back up the chain. Then convert array into a list of
    # expressions to use for a new OR expression.

    orExpr <- as.list(do.call(c, do.call(c, from@.Data)))
    as(orExpr, to)
  }
)

# Coercing to andExpr
setAs(from = "orExpr", to = "andExpr", def = simpleCoerceToList)

setAs(from = ".expr", to = "andExpr", def = function(from, to) {
  as(as(from, "orExpr"), "andExpr")
})

# Coercion to .filter subclasses

# Consider having coercision just to .tableFilter where coercion requires
# call to initialise where the object is coerced to a specific type of .tableFilter
# class, i.e GA, RT or MCF subclasses.

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

setAs(from = "NULL", to = ".tableFilter", def = coerceViaList)

setAs(from = "andExpr", to = "gaFilter", def = simpleCoerce)
setAs(from = "andExpr", to = "mcfFilter", def = simpleCoerce)
setAs(from = "andExpr", to = "rtFilter", def = simpleCoerce)

setAs(from = "orExpr", to = "gaFilter", def = coerceViaAnd)
setAs(from = "orExpr", to = "mcfFilter", def = coerceViaAnd)
setAs(from = "orExpr", to = "rtFilter", def = coerceViaAnd)
setAs(from = "orExpr", to = ".tableFilter", def = coerceViaAnd)

setAs(from = ".expr", to = "gaFilter", def = coerceViaAnd)
setAs(from = ".expr", to = "mcfFilter", def = coerceViaAnd)
setAs(from = ".expr", to = "rtFilter", def = coerceViaAnd)
setAs(from = ".expr", to = ".tableFilter", def = coerceViaAnd)

setAs(from = "gaSegmentCondition", to = ".tableFilter", def = simpleCoerceData)

#############\/ Transform to method of TableFilter and TableFilter<- generic functions

setAs(from = ".query", to = ".tableFilter",
  def = function(from, to){
    from@filters
  },
  replace = function(from, value) {
    use_class <- class(from@filters)
    from@filters <- as(value, use_class)
    validObject(from)
    from
  }
)

### Review the following coercions using "new"
# Coercion to custom segment classes

setAs(from = ".compoundExpr", to = "gaSegmentSequenceStep", def = function(from, to) {
  new(to, as(from, "andExpr"))
})

setAs(from = ".compoundExpr", to = "gaSegmentSequenceFilter", def = function(from, to) {
  new(to, as(as(from, "gaSegmentSequenceStep"), "andExpr"))
})

# Coercing to gaSegmentConditionFilter
setAs(from = ".compoundExpr", to = "gaSegmentConditionFilter", def = function(from, to) {
  new(to, as(from, "andExpr"))
})

# Coercing to gaSegmentFilterList
setAs(from = ".compoundExpr", to = "gaSegmentFilterList", def = function(from, to) {
  new(to, list(as(from, "gaSegmentConditionFilter")))
})

# Coercion to gaSegmentId

setAs(from = "character", to = "gaSegmentId", def = simpleCoerce)

setAs(from = "numeric", to = "gaSegmentId", def = function(from, to) {
  new(to, as.character(from))
})

setAs(from = "gaUserSegment", to = "gaSegmentId", def = function(from, to) {
  new(to, Segment(from))
})

# Coercing to gaDynSegment

setAs(from = "gaFilter", to = "gaDynSegment", def = simpleCoerceData)

setAs(from = "orExpr", to = "gaDynSegment", def = function(from, to) {
  as(as(from, "andExpr"), to)
})

#Review this coercion method
setAs(from = "andExpr", to = "gaDynSegment", def = function(from, to) {
  new(to, list(GaSegmentFilters(GaCondition(from))))
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

#############\/ Transform to method of Segment and Segment<- generic functions
setAs(from = "gaQuery", to = ".gaSegment",
  def = function(from) {
    from@segments
  },
  replace = function(from, value) {
    from@segments <- as(value, ".gaSegment") # Need to define coercions to .gaSegment from char and numeric
    validObject(from)
    from
  }
)

# Coercion to .varList
setAs(from = "list", to = ".varList", def = function(from) {
  vars <- unique(lapply(from, as, Class = ".var"))
  if (length(vars) >= 1) {
    if (is(vars[[1]], ".gaVar")) {
      as(vars, ".gaVarList")
    } else if (is(vars[[1]], ".mcfVar")) {
      as(vars, ".mcfVarList")
    } else if (is(vars[[1]], ".rtVar")) {
      as(vars, ".rtVarList")
    } else stop("Cannot determine type of vars in list")
  } else {
    new(to, vars)
  }
})

setAs(from = "list", to = ".dimensions", def = function(from, to) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".dimVar")}))
  if (length(vars) >= 1) {
    if (is(vars[[1]], ".gaVar")) {
      as(vars, "gaDimensions")
    } else if (is(vars[[1]], ".mcfVar")) {
      as(vars, "mcfDimensions")
    } else if (is(vars[[1]], ".rtVar")) {
      as(vars, "rtDimensions")
    } else stop("Cannot determine type of vars in list")
  } else {
    new(to, vars)
  }
})

updateSortBy <- function(object) {
  queryVars <- union(object@dimensions, object@metrics)
  curSortVars <- object@sortBy
  newSortVars <- intersect(curSortVars, queryVars)
  desc <- as.logical(curSortVars@desc[curSortVars %in% newSortVars])
  use_class <- class(object@sortBy)
  object@sortBy <- new(use_class, newSortVars, desc = desc)
  object
}

#############\/ Transform to method of Dimensions and Dimensions<- generic functions
setAs(from = ".query", to = ".dimensions",
  def = function(from, to) {
    from@dimensions
  },
  replace = function(from, value) {
    use_class <- class(from@dimensions)
    from@dimensions <- as(value, use_class)
    from <- ganalytics:::updateSortBy(from)
    validObject(from)
    from
  }
)

setAs(from = "list", to = ".metrics", def = function(from) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".metVar")}))
  if (length(vars) >= 1) {
    if (is(vars[[1]], ".gaVar")) {
      as(vars, "gaMetrics")
    } else if (is(vars[[1]], ".mcfVar")) {
      as(vars, "mcfMetrics")
    } else if (is(vars[[1]], ".rtVar")) {
      as(vars, "rtMetrics")
    } else stop("Cannot determine type of vars in list")
  } else {
    new(to, vars)
  }
})

#############\/ Transform to method of Metrics and Metrics<- generic functions
setAs(from = ".query", to = ".metrics",
  def = function(from, to) {
    from@metrics
  },
  replace = function(from, value) {
    use_class <- class(from@metrics)
    from@metrics <- as(value, use_class)
    from <- ganalytics:::updateSortBy(from)
    validObject(from)
    from
  }
)

setAs(from = "list", to = ".sortBy", def = function(from) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".var")}))
  if (length(vars) >= 1) {
    if (is(vars[[1]], ".gaVar")) {
      as(vars, "gaSortBy")
    } else if (is(vars[[1]], ".mcfVar")) {
      as(vars, "mcfSortBy")
    } else if (is(vars[[1]], ".rtVar")) {
      as(vars, "rtSortBy")
    } else stop("Cannot determine type of vars in list")
  } else {
    new(to, vars)
  }
})

#############\/ Transform to method of SortBy and SortBy<- generic functions
setAs(from = ".query", to = ".sortBy",
  def = function(from, to) {
    from@sortBy
  },
  replace = function(from, value) {
    use_class <- class(from@sortBy)
    from@sortBy <- as(value, use_class)
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

setAs(from = "character", to = ".dimensions", def = coerceViaList)
setAs(from = "character", to = ".metrics", def = coerceViaList)

setAs(from = "NULL", to = ".dimensions", def = coerceViaList)
setAs(from = "NULL", to = ".metrics", def = coerceViaList)
setAs(from = "NULL", to = ".sortBy", def = coerceViaList)
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
  desc <- logical(length(vars))
  desc[grep("^\\+", varChars)] <- FALSE
  desc[grep("^\\-", varChars)] <- TRUE
  vars@desc <- desc
  validObject(vars)
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

#######\/ Consider changing to generic methods View and View<- rather than coercion
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

#######\/ Consider changing to generic methods rather than coercion
# Select the default view of the property
setAs(from = "gaProperty", to = "viewId",
  def = function(from, to) {
    defaultView <- from$defaultView
    if (length(defaultView) == 0) {
      defaultView <- from$views$entities[[1]]
    }
    as(defaultView, "viewId")
  },
  replace = function(from, value) {
    as(from$defaultView, "viewId") <- as(value, "viewId")
  }
)

#######\/ Consider changing to generic methods rather than coercion
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

#############\/ Transform to method of DateRange and DateRange<- generic functions

# Coercion to dateRange
setAs(from = "Date", to = "dateRange",
  def = function(from, to) {
    assert_that(length(from) == 2)
    startDate = from[1]
    endDate = from[2]
    new("dateRange", startDate, endDate)
  }
)

setAs(from = "Interval", to = "dateRange",
  def = function(from, to) {
    date_range_char <- str_split_fixed(as.character(from), "--", 2)
    start_date <- as.Date(date_range_char[, 1])
    end_date <- as.Date(date_range_char[, 2])
    if (start_date <= end_date) {
      new(to, start_date, end_date)
    } else {
      new(to, end_date, start_date)
    }
  }
)

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
        segments <- as(from, ".gaSegment")
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
          "segment" = if(length(segments) >= 1) {
            as(segments, "character")
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

setAs(
  from = "rtQuery",
  to = "matrix",
  def = function(from) {
    views <- do.call(
      what = rbind,
      args = lapply(
        X = as(from, "viewId"),
        FUN = function(viewId) {
          data.frame(
            viewId = viewId,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    params <- mapply(
      FUN = function(viewId) {
        metrics <- as(from, ".metrics")
        dimensions <- as(from, ".dimensions")
        sortBy <- as(from, ".sortBy")
        tableFilter <- as(from, ".tableFilter")
        c(
          "ids" = as(viewId, "character"),
          "metrics" = as(metrics, "character"),
          "dimensions" = if(length(dimensions) >= 1) {
            as(dimensions, "character")
          },
          "sort" = if(length(sortBy) >= 1) {
            as(sortBy, "character")
          },
          "filters" = if(length(tableFilter) >= 1) {
            as(tableFilter, "character")
          }
        )
      },
      views$viewId
    )
  }
)
