#' @include utils.R
NULL

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
  } else if (all_inherit(vars, ".dimVar")) {
    as(vars, "gaDimensions")
  } else {
    as(vars, "gaSortBy")
  }
})

setAs(from = "list", to = ".mcfVarList", def = function(from) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".mcfVar")}))
  if (all_inherit(vars, ".metVar")) {
    as(vars, "mcfMetrics")
  } else if (all_inherit(vars, ".dimVar")) {
    as(vars, "mcfDimensions")
  } else {
    as(vars, "mcfSortBy")
  }
})

setAs(from = "list", to = ".rtVarList", def = function(from) {
  vars <- unique(lapply(from, function(var) {as(as.character(var), ".rtVar")}))
  if (all_inherit(vars, ".metVar")) {
    as(vars, "rtMetrics")
  } else if (all_inherit(vars, ".dimVar")) {
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

