#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# ---- Operator, GaDimOperator, GaMetOperator ----

setMethod("Operator", ".operator", function(.Object) {.Object})

setMethod(
  f = "Operator<-",
  signature = c(".operator", "character"),
  definition = function(.Object, value) {
    as(.Object, "character") <- value
    .Object
  }
)

setMethod("Operator", ".expr", function(.Object) {as(.Object, ".operator")})

setMethod(
  f = "Operator<-",
  signature = ".expr",
  definition = function(.Object, value) {
    as(.Object, ".operator") <- value
    .Object
  }
)

# ---- IsRegEx ----

setMethod("IsRegEx", ".dimOperator", function(.Object) {.Object %in% c("=~", "!~")})

setMethod("IsRegEx", ".expr", function(.Object) {IsRegEx(Operator(.Object))})
