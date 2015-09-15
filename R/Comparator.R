#' @include comparator-classes.R
#' @include expression-classes.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include comparator-coerce.R
#' @include utils.R
NULL

# ---- Comparator, GaDimComparator, GaMetComparator ----

#' @describeIn Comparator
setMethod("Comparator", ".comparator", function(object) {object})

#' @describeIn Comparator
setMethod(
  f = "Comparator<-",
  signature = c(".comparator", "character"),
  definition = function(object, value) {
    as(object, "character") <- value
    object
  }
)

#' @describeIn Comparator
setMethod("Comparator", ".expr", function(object) {as(object, ".comparator")})

#' @describeIn Comparator
setMethod(
  f = "Comparator<-",
  signature = ".expr",
  definition = function(object, value) {
    as(object, ".comparator") <- value
    object
  }
)

# ---- IsRegEx ----
#' @describeIn IsRegEx
setMethod("IsRegEx", ".dimComparator", function(object) {object %in% c("=~", "!~")})

#' @describeIn IsRegEx
setMethod("IsRegEx", ".expr", function(object) {IsRegEx(Comparator(object))})
