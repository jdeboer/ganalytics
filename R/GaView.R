#' @include all-classes.R
#' @include management-api-classes.R
#' @include all-coercions.R
#' @include init-methods.R
#' @include all-generics.R
NULL

# -- GaView ----
#' @describeIn GaView
setMethod("GaView", "viewId", function(object) {object})

#' @describeIn GaView
setMethod("GaView", "gaView", function(object) {as(object, "viewId")})

#' @describeIn GaView
setMethod("GaView", "gaProperty", function(object) {as(object, "viewId")})

#' @describeIn GaView
setMethod("GaView", "gaAccount", function(object) {as(object, "viewId")})

#' @describeIn GaView
setMethod("GaView", "character", function(object) {as(object, "viewId")})

#' @describeIn GaView
setMethod("GaView", "numeric", function(object) {as(object, "viewId")})

#' @describeIn GaView
setMethod("GaView", ".query", function(object) {as(object, "viewId")})

#' @describeIn GaView
setMethod(
  f = "GaView<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, "viewId") <- value
    object
  }
)

