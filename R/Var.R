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
  definition = function(object) {
    as(object, ".var", strict = FALSE)
  }
)

setMethod(
  f = "Var<-",
  signature = c(".var", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

setMethod("Var", ".expr", function(object) {as(object, ".var")})

setMethod(
  f = "Var<-",
  signature = c(".expr", "character"),
  definition = function(object, value) {
    as(object, ".var") <- value
    object
  }
)

setMethod("Var", ".gaVarList", function(object) {object})

## Create a gaMet or gaDim object
## GaVar takes a GA variable and determines whether to return a Dimension or Metric object
setMethod("GaVar", "character", function(object) {as(object, ".gaVar")})

setMethod(
  f = "GaVar<-",
  signature = c(".gaVar", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

setMethod("GaVar", ".expr", function(object) {as(object, ".gaVar")})

setMethod(
  f = "GaVar<-",
  signature = c(".expr", "character"),
  definition = function(object, value) {
    as(object, ".gaVar") <- value
    object
  }
)

setMethod("GaVar", ".gaVarList", function(object) {object})

# ---- McfVar ----

setMethod("McfVar", ".mcfVar", function(object) {object})

## Create a mcfMet or mcfDim object
## McfVar takes a MCF variable and determines whether to return a Dimension or Metric object
setMethod("McfVar", "character", function(object) {as(object, ".mcfVar")})

# ---- RtVar ----

setMethod("RtVar", ".rtVar", function(object) {object})

## Create a rtMet or rtDim object
## McfVar takes a RT variable and determines whether to return a Dimension or Metric object
setMethod("RtVar", "character", function(object) {as(object, ".rtVar")})

# -- GaMetrics ----

setMethod(
  f = "Metrics",
  signature = "NULL",
  definition = function(.Object, ...) {
    vars <- ArgList(.Object, ...)
    as(vars, ".metrics")
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

setMethod("Metrics", ".query", function(.Object) {as(.Object, ".metrics")})

setMethod(
  f = "Metrics<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    as(.Object, ".metrics") <- value
    .Object
  }
)

# -- GaDimensions ----

setMethod(
  f = "Dimensions",
  signature = "NULL",
  definition = function(.Object, ...) {
    vars <- ArgList(.Object, ...)
    as(vars, ".dimensions")
  }
)

setMethod(
  f = "Dimensions",
  signature = ".dimensions",
  definition = function(.Object, ...) {
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

setMethod("Dimensions", ".query", function(.Object) {as(.Object, ".dimensions")})

setMethod(
  f = "Dimensions<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    as(.Object, ".dimensions") <- value
    .Object
  }
)

# Backwards compatibility
#'@export GaDimensions
GaDimensions <- Dimensions
#'@export GaMetrics
GaMetrics <- Metrics
#'@export GaDimensions<-
`GaDimensions<-` <- `Dimensions<-`
#'@export GaMetrics<-
`GaMetrics<-` <- `Metrics<-`

