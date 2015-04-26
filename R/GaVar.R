#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
#' @include ganalytics-package.R
#' @include meta.R
NULL

# ---- GaVar ----

setMethod(
  f = "GaVar",
  signature = ".gaVar",
  definition = function(.Object) {
    return(.Object)
  }
)

## Create a gaMet or gaDim object
## GaVar takes a GA variable and determines whether to return a Dimension or Metric object
setMethod(
  f = "GaVar",
  signature = "character",
  definition = function(.Object) {
    var <- tryCatch(
      new("gaMetVar", .Object),
      error = function(e) {
        tryCatch(
          new("gaDimVar", .Object),
          error = function(e) {
            stop(e)
          }
        )
      }
    )
    return (var)
  }
)

setMethod(
  f = "GaVar<-",
  signature = c(".gaVar", "character"),
  definition = function(.Object, value) {
    as(.Object, "character") <- value
    return(.Object)
  }
)

setMethod(
  f = "GaVar",
  signature = ".expr",
  definition = function(.Object) {
    GaVar(.Object@var)
  }
)

setMethod(
  f = "GaVar<-",
  signature = ".expr",
  definition = function(.Object, value) {
    GaVar(.Object@var) <- value
    return(.Object)
  }
)

setMethod(
  f = "GaVar",
  signature = ".gaVarList",
  definition = function(.Object) {
    return(.Object@.Data)
  }
)

# ---- McfVar ----

setMethod(
  f = "McfVar",
  signature = ".mcfVar",
  definition = function(.Object) {
    return(.Object)
  }
)

## Create a mcfMet or mcfDim object
## McfVar takes a MCF variable and determines whether to return a Dimension or Metric object
setMethod(
  f = "McfVar",
  signature = "character",
  definition = function(.Object) {
    var <- tryCatch(
      new("mcfMetVar", .Object),
      error = function(e) {
        tryCatch(
          new("mcfDimVar", .Object),
          error = function(e) {
            stop(e)
          }
        )
      }
    )
    return (var)
  }
)

# ---- RtVar ----

setMethod(
  f = "RtVar",
  signature = ".rtVar",
  definition = function(.Object) {
    return(.Object)
  }
)

## Create a rtMet or rtDim object
## McfVar takes a RT variable and determines whether to return a Dimension or Metric object
setMethod(
  f = "RtVar",
  signature = "character",
  definition = function(.Object) {
    var <- tryCatch(
      new("rtMetVar", .Object),
      error = function(e) {
        tryCatch(
          new("rtDimVar", .Object),
          error = function(e) {
            stop(e)
          }
        )
      }
    )
    return (var)
  }
)

# -- GaMetrics ----

setMethod(
  f = "GaMetrics",
  signature = "gaMetrics",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaMetrics",
  signature = "character",
  definition = function(.Object, ...) {
    vars <- unique(
      lapply(
        X = ArgList(.Object, ...),
        FUN = function(x) {
          new(Class = "gaMetVar", x)
        }
      )
    )
    new(Class = "gaMetrics", vars)
  }
)

setMethod(
  f = "GaMetrics",
  signature = "list",
  definition = function(.Object) {
    GaMetrics(as.character(.Object))
  }
)

# -- GaDimensions ----

setMethod(
  f = "GaDimensions",
  signature = "gaDimensions",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaDimensions",
  signature = "character",
  definition = function(.Object, ...) {
    vars <- unique(
      lapply(
        X = ArgList(.Object, ...),
        FUN = function(x) {
          new(Class = "gaDimVar", x)
        }
      )
    )
    new(Class = "gaDimensions", vars)
  }
)

setMethod(
  f = "GaDimensions",
  signature = "list",
  definition = function(.Object, ...) {
    GaDimensions(as.character(.Object), ...)
  }
)

setMethod(
  f = "GaDimensions",
  signature = "NULL",
  definition = function(.Object) {
    new("gaDimensions", list())
  }
)


setMethod(
  f = "GaMetrics",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@metrics
  }
)

setMethod(
  f = "GaMetrics<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@metrics <- GaMetrics(value)
    queryVars <- union(GaDimensions(.Object), GaMetrics(.Object))
    curSort <- GaSortBy(.Object)
    newSortVars <- intersect(curSort, GaSortBy(queryVars))
    GaSortBy(.Object) <- GaSortBy(
      newSortVars,
      desc = if(!is.null(curSort)) {
        curSort@desc[as.character(curSort) == as.character(newSortVars)]
      } else {
        logical(0)
      }
    )
    validObject(.Object)
    return(.Object)
  }
)

setMethod(
  f = "GaDimensions",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@dimensions
  }
)

setMethod(
  f = "GaDimensions<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@dimensions <- GaDimensions(value)
    queryVars <- union(GaDimensions(.Object), GaMetrics(.Object))
    curSort <- GaSortBy(.Object)
    newSortVars <- intersect(curSort, GaSortBy(queryVars))
    GaSortBy(.Object) <- GaSortBy(
      newSortVars,
      desc = if(!is.null(curSort)) {
        curSort@desc[as.character(curSort) == as.character(newSortVars)]
      } else {
        logical(0)
      }
    )
    validObject(.Object)
    return(.Object)
  }
)
