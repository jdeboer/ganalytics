#' @include var-classes.R
#' @include var-coerce.R
#' @include expr-classes.R
#' @include var-list-classes.R
#' @include var-list-coerce.R
#' @include query-classes.R
#' @include init-methods.R
#' @include Var-generics.R
#' @include Var-list-generics.R
#' @include utils.R
#' @importFrom methods new setMethod
NULL

#' @describeIn Var Coerce a character to '.var'.
setMethod(
  f = "Var",
  signature = "character",
  definition = function(object) {
    as(object, ".var", strict = FALSE)
  }
)

#' @describeIn Var Set a '.var' object to a new value coerced from character.
setMethod(
  f = "Var<-",
  signature = c(".var", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

#' @describeIn Var Get the variable of an expression object.
setMethod("Var", ".expr", function(object) {as(object, ".var")})

#' @describeIn Var Set the variable of an expression object using a character value to be coerced to '.var'.
setMethod(
  f = "Var<-",
  signature = c(".expr", "character"),
  definition = function(object, value) {
    as(object, ".var") <- value
    object
  }
)

#' @describeIn Var Get the variables within a variable list object, such as sortBy, dimensions or metrics.
setMethod("Var", ".gaVarList", function(object) {object})

# Create a gaMet or gaDim object.

#' @describeIn Var GaVar takes a GA variable name and determines whether to return a Dimension or Metric object
setMethod("GaVar", "character", function(object) {as(object, ".gaVar")})

#' @describeIn Var Set the Var of a gaExpr object.
setMethod(
  f = "GaVar<-",
  signature = c(".gaVar", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

#' @describeIn Var Get the variable from expression object coerced to '.garVar'.
setMethod("GaVar", ".expr", function(object) {as(object, ".gaVar")})

#' @describeIn Var Set the variable of an expression to a .gaVar as named by a character value.
setMethod(
  f = "GaVar<-",
  signature = c(".expr", "character"),
  definition = function(object, value) {
    as(object, ".gaVar") <- value
    object
  }
)

#' @describeIn Var Get the variables of a .gaVarList.
setMethod("GaVar", ".gaVarList", function(object) {object})

# ---- McfVar ----

#' @describeIn Var McfVar takes a MCF variable and determines whether to return a Dimension or Metric object
setMethod("McfVar", "ANY", function(object) {as(object, ".mcfVar")})

# ---- RtVar ----

#' @describeIn Var McfVar takes a RT variable and determines whether to return a Dimension or Metric object
setMethod("RtVar", "ANY", function(object) {as(object, ".rtVar")})

# -- GaMetrics ----

#' @describeIn Metrics Coerce one or more supplied objects to .metrics.
setMethod(
  f = "Metrics",
  signature = "ANY",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".metrics")
  }
)

#' @describeIn Metrics Get the list of metrics for a '.query'.
setMethod("Metrics", ".query", function(object) {as(object, ".metrics")})

#' @describeIn Metrics Set the metrics for a '.query' object.
setMethod(
  f = "Metrics<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, ".metrics") <- value
    object
  }
)

# -- Dimensions ----

#' @describeIn Dimensions Coerces the supplied character vector or list into a vector of
#'   Google Analytics dimensions.
#' @export
setMethod(
  f = "Dimensions",
  signature = "ANY",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".dimensions")
  }
)

#' @describeIn Dimensions Returns the dimensions used within the supplied query.
#' @export
setMethod("Dimensions", ".query", function(object) {as(object, ".dimensions")})

#' @describeIn Dimensions Replace the dimensions of the query.
#' @export
setMethod(
  f = "Dimensions<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, ".dimensions") <- value
    object
  }
)
