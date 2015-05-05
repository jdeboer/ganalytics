#' @include all-classes.R
#' @include management-api-classes.R
#' @include all-coercions.R
#' @include init-methods.R
#' @include all-generics.R
NULL

# -- GaView ----

setMethod("GaView", "viewId", function(.Object) {.Object})
setMethod("GaView", "gaView", function(.Object) {as(.Object, "viewId")})
setMethod("GaView", "gaProperty", function(.Object) {as(.Object, "viewId")})
setMethod("GaView", "gaAccount", function(.Object) {as(.Object, "viewId")})
setMethod("GaView", "character", function(.Object) {as(.Object, "viewId")})
setMethod("GaView", "numeric", function(.Object) {as(.Object, "viewId")})
setMethod("GaView", ".query", function(.Object) {as(.Object, "viewId")})

setMethod(
  f = "GaView<-",
  signature = c(".query", "ANY"),
  definition = function(.Object, value) {
    as(.Object, "viewId") <- value
    .Object
  }
)

# Backwards compatibility
#'@export GaProfileId
GaProfileId <- GaView
#'@export GaProfileId<-
`GaProfileId<-` <- `GaView<-`
