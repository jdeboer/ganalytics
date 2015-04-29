#' @include all-classes.R
#' @include ganalytics-package.R
#' @include management-api-classes.R
#' @include init-methods.R
#' @include all-generics.R
#' @include all-coercions.R
#' @include helper-functions.R
NULL

# -- GaView ----

setMethod(
  f = "GaView",
  signature = "viewId",
  definition = function(.Object) {
    return(.Object)
  }
)

setMethod(
  f = "GaView",
  signature = "gaView",
  definition = function(.Object) {
    GaView(.Object$id)
  }
)

# Passing a property object to GaQuery will select the default view of that property
setMethod(
  f = "GaView",
  signature = "gaProperty",
  definition = function(.Object) {
    GaView(.Object$defaultView)
  }
)

# Passing an account object to GaQuery will select the first property of that account
# which is then used to select a view (as per above).
setMethod(
  f = "GaView",
  signature = "gaAccount",
  definition = function(.Object) {
    GaView(.Object$properties$entities[[1]])
  }
)

setMethod(
  f = "GaView",
  signature = "character",
  definition = function(.Object) {
    new(Class = "viewId", .Object)
  }
)

setMethod(
  f = "GaView",
  signature = "numeric",
  definition = function(.Object) {
    GaView(
      as.character(.Object)
    )
  }
)


setMethod(
  f = "GaView",
  signature = "gaQuery",
  definition = function(.Object) {
    .Object@viewId
  }
)

setMethod(
  f = "GaView<-",
  signature = c("gaQuery", "ANY"),
  definition = function(.Object, value) {
    .Object@viewId <- GaView(value)
    validObject(.Object)
    return(.Object)
  }
)


#'@export GaProfileId
GaProfileId <- GaView
#'@export GaProfileId<-
`GaProfileId<-` <- `GaView<-`
