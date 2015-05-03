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
  f = "Var",
  signature = "character",
  definition = function(.Object) {
    as(.Object, ".var")
  }
)

setMethod(
  f = "Var<-",
  signature = c(".var", "character"),
  definition = function(.Object, value) {
    as(.Object, "character") <- value
    .Object
  }
)

setMethod(
  f = "Var",
  signature = ".expr",
  definition = function(.Object) {
    as(.Object, ".var")
  }
)

setMethod(
  f = "Var<-",
  signature = c(".expr", "character"),
  definition = function(.Object, value) {
    GaVar(.Object@var) <- value
    .Object
  }
)

setMethod(
  f = "Var",
  signature = ".gaVarList",
  definition = function(.Object) {
    .Object@.Data
  }
)

## Create a gaMet or gaDim object
## GaVar takes a GA variable and determines whether to return a Dimension or Metric object
setMethod(
  f = "GaVar",
  signature = "character",
  definition = function(.Object) {
    as(.Object, ".gaVar")
  }
)

setMethod(
  f = "GaVar<-",
  signature = c(".gaVar", "character"),
  definition = function(.Object, value) {
    as(.Object, "character") <- value
    .Object
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
  signature = c(".expr", "character"),
  definition = function(.Object, value) {
    GaVar(.Object@var) <- value
    .Object
  }
)

setMethod(
  f = "GaVar",
  signature = ".gaVarList",
  definition = function(.Object) {
    .Object@.Data
  }
)

# ---- McfVar ----

setMethod(
  f = "McfVar",
  signature = ".mcfVar",
  definition = function(.Object) {
    .Object
  }
)

## Create a mcfMet or mcfDim object
## McfVar takes a MCF variable and determines whether to return a Dimension or Metric object
setMethod(
  f = "McfVar",
  signature = "character",
  definition = function(.Object) {
    as(.Object, ".mcfVar")
  }
)

# ---- RtVar ----

setMethod(
  f = "RtVar",
  signature = ".rtVar",
  definition = function(.Object) {
    .Object
  }
)

## Create a rtMet or rtDim object
## McfVar takes a RT variable and determines whether to return a Dimension or Metric object
setMethod(
  f = "RtVar",
  signature = "character",
  definition = function(.Object) {
    as(.Object, ".rtVar")
  }
)

# -- GaMetrics ----

setMethod(
  f = "Metrics",
  signature = "NULL",
  definition = function(.Object) {
    vars <- ArgList(.Object, ...)
    new(".metrics", vars)
  }
)

setMethod(
  f = "Metrics",
  signature = ".metrics",
  definition = function(.Object, ...) {
    vars <- ArgList(.Object, ...)
    as(vars, ".metrics")
  }
)

setMethod(
  f = "Metrics",
  signature = "character",
  definition = function(.Object, ...) {
    vars <- ArgList(.Object, ...)
    as(vars, ".metrics")
  }
)

setMethod(
  f = "Metrics",
  signature = "list",
  definition = function(.Object, ...) {
    vars <- ArgList(.Object, ...)
    as(vars, ".metrics")
  }
)

setMethod(
  f = "Metrics",
  signature = ".query",
  definition = function(.Object) {
    as(.Object, ".metrics")
  }
)

# -- GaDimensions ----

setMethod(
  f = "Dimensions",
  signature = "NULL",
  definition = function(.Object) {
    vars <- ArgList(.Object, ...)
    as(vars, ".dimensions")
  }
)

setMethod(
  f = "Dimensions",
  signature = ".dimensions",
  definition = function(.Object) {
    vars <- ArgList(.Object, ...)
    as(vars, ".dimensions")
  }
)

setMethod(
  f = "Dimensions",
  signature = "character",
  definition = function(.Object, ...) {
    vars <- ArgList(.Object, ...)
    as(vars, ".dimensions")
  }
)

setMethod(
  f = "Dimensions",
  signature = "list",
  definition = function(.Object, ...) {
    vars <- ArgList(.Object, ...)
    as(vars, ".dimensions")
  }
)

setMethod(
  f = "Dimensions",
  signature = ".query",
  definition = function(.Object) {
    as(.Object, ".dimensions")
  }
)

# Backwards compatibility
#'@export GaDimensions
GaDimensions <- Dimensions
#'@export GaMetrics
GaMetrics <- Metrics
#'@export GaDimensions<-
`GaDimensions<-` <- Dimensions
#'@export GaMetrics<-
`GaMetrics<-` <- Metrics

