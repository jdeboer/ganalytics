#' @include all-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include ganalytics-package.R
#' @include helper-functions.R
NULL

# ---- Operator, GaDimOperator, GaMetOperator ----

setMethod("Operator", ".operator", function(object) {object})

setMethod(
  f = "Operator<-",
  signature = c(".operator", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

setMethod("Operator", ".expr", function(object) {as(object, ".operator")})

setMethod(
  f = "Operator<-",
  signature = ".expr",
  definition = function(object, value) {
    as(object, ".operator") <- value
    object
  }
)

# ---- IsRegEx ----

setMethod("IsRegEx", ".dimOperator", function(object) {object %in% c("=~", "!~")})

setMethod("IsRegEx", ".expr", function(object) {IsRegEx(Operator(object))})
