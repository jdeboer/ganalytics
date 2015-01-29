#' @include all-classes.R
#' @include ganalytics-package.R
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

# Passing a property object to GaQuery will select the default view of that property
setMethod(
  f = "GaProfileId",
  signature = "gaProperty",
  definition = function(.Object) {
    GaProfileId(.Object$defaultView)
  }
)

# Passing an account object to GaQuery will select the first property of that account
# which is then used to select a view (as per above).
setMethod(
  f = "GaProfileId",
  signature = "gaAccount",
  definition = function(.Object) {
    GaProfileId(.Object$properties$entities[[1]])
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
