#' @include all-classes.R
#' @include management-api-classes.R
#' @include all-coercions.R
#' @include init-methods.R
#' @include all-generics.R
NULL

# -- GaView ----

setMethod("GaView", "viewId", function(object) {object})
setMethod("GaView", "gaView", function(object) {as(object, "viewId")})
setMethod("GaView", "gaProperty", function(object) {as(object, "viewId")})
setMethod("GaView", "gaAccount", function(object) {as(object, "viewId")})
setMethod("GaView", "character", function(object) {as(object, "viewId")})
setMethod("GaView", "numeric", function(object) {as(object, "viewId")})
setMethod("GaView", ".query", function(object) {as(object, "viewId")})

setMethod(
  f = "GaView<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, "viewId") <- value
    object
  }
)

# Backwards compatibility
#'@export GaProfileId
GaProfileId <- GaView
#'@export GaProfileId<-
`GaProfileId<-` <- `GaView<-`
