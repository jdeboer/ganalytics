#' @include all-coercions.R
#' @include var-classes.R
#' @include var-coerce.R
#' @include expression-classes.R
#' @include var-list-classes.R
#' @include var-list-coerce.R
#' @include query-classes.R
#' @include init-methods.R
#' @include all-generics.R
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

#' @describeIn Var
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

#' @describeIn Var Get the value of a .mcfVar object.
setMethod("McfVar", ".mcfVar", function(object) {object})

# Create a mcfMet or mcfDim object

#' @describeIn Var McfVar takes a MCF variable and determines whether to return a Dimension or Metric object
setMethod("McfVar", "character", function(object) {as(object, ".mcfVar")})

# ---- RtVar ----

#' @describeIn Var Get the value of a .rtVar object.
setMethod("RtVar", ".rtVar", function(object) {object})

# Create a rtMet or rtDim object

#' @describeIn Var McfVar takes a RT variable and determines whether to return a Dimension or Metric object
setMethod("RtVar", "character", function(object) {as(object, ".rtVar")})

# -- GaMetrics ----

#' @describeIn Metrics Return an empty .metrics object
setMethod(
  f = "Metrics",
  signature = "NULL",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".metrics")
  }
)

#' @describeIn Metrics Return the value of a .metrics object or concatenated .metrics objects
setMethod(
  f = "Metrics",
  signature = ".metrics",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".metrics")
  }
)

#' @describeIn Metrics Coerce one or more supplied character values to .metrics.
setMethod(
  f = "Metrics",
  signature = "character",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".metrics")
  }
)

#' @describeIn Metrics Coerce one or more supplied lists to .metrics
setMethod(
  f = "Metrics",
  signature = "list",
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

#' @describeIn Dimensions
#' @export
setMethod(
  f = "Dimensions",
  signature = "NULL",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".dimensions")
  }
)

#' @describeIn Dimensions
#' @export
setMethod(
  f = "Dimensions",
  signature = ".dimensions",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".dimensions")
  }
)

#' @describeIn Dimensions
#' @export
setMethod(
  f = "Dimensions",
  signature = "character",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".dimensions")
  }
)

#' @describeIn Dimensions
#' @export
setMethod(
  f = "Dimensions",
  signature = "list",
  definition = function(object, ...) {
    vars <- ArgList(object, ...)
    as(vars, ".dimensions")
  }
)

#' @describeIn Dimensions
#' @export
setMethod("Dimensions", ".query", function(object) {as(object, ".dimensions")})

#' @describeIn Dimensions
#' @export
setMethod(
  f = "Dimensions<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, ".dimensions") <- value
    object
  }
)
