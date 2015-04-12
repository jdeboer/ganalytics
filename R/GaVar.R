#' @include all-coercions.R
#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include helper-functions.R
#' @include ganalytics-package.R
#' @include meta.R
NULL

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
    gaVar <- tryCatch(
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
    return (gaVar)
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

# ---- GaVar ----

setMethod(
  f = "GaVar",
  signature = ".gaExpr",
  definition = function(.Object) {
    GaVar(.Object@gaVar)
  }
)

setMethod(
  f = "GaVar<-",
  signature = ".gaExpr",
  definition = function(.Object, value) {
    GaVar(.Object@gaVar) <- value
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
