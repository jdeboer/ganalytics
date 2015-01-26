#' @include all-classes.R
#' @include management-api-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
NULL

# -- GaProfileId ----

setMethod(
  f = "GaProfileId",
  signature = "gaProfileId",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaProfileId",
  signature = "gaView",
  definition = function(.Object) {
    GaProfileId(.Object$id)
  }
)

setMethod(
  f = "GaProfileId",
  signature = "gaProperty",
  definition = function(.Object) {
    GaProfileId(.Object$defaultView)
  }
)

setMethod(
  f = "GaProfileId",
  signature = "character",
  definition = function(.Object) {
    new(Class = "gaProfileId", .Object)
  }
)

setMethod(
  f = "GaProfileId",
  signature = "numeric",
  definition = function(.Object) {
    GaProfileId(
      as.character(.Object)
    )
  }
)


setMethod(
  f = "GaProfileId",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@profileId
  }
)

setMethod(
  f = "GaProfileId<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@profileId <- GaProfileId(value)
    validObject(.Object)
    return(.Object)
  }
)
