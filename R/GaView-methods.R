#' @include query-classes.R
#' @include management-api-classes.R
#' @include view-coerce.R
#' @include Query-generics.R
#' @importFrom methods setMethod as
NULL

# -- GaView ----
#' @describeIn GaView Returns itself
setMethod("GaView", "viewId", function(object) {object})

#' @describeIn GaView Returns the ID of the supplied view.
setMethod("GaView", "gaView", function(object) {as(object, "viewId")})

#' @describeIn GaView Returns the ID of the first view within the supplied
#'   property.
setMethod("GaView", "gaProperty", function(object) {as(object, "viewId")})

#' @describeIn GaView Returns the ID of the first view within the first property
#'   of the supplied account.
setMethod("GaView", "gaAccount", function(object) {as(object, "viewId")})

#' @describeIn GaView Parses the supplied character value as a valid view ID.
setMethod("GaView", "character", function(object) {as(object, "viewId")})

#' @describeIn GaView Parses the supplied numeric value as a valid view ID.
setMethod("GaView", "numeric", function(object) {as(object, "viewId")})

#' @describeIn GaView Returns the view ID of the given query.
setMethod("GaView", ".query", function(object) {as(object, "viewId")})

#' @describeIn GaView Replace the view being used by a query.
setMethod(
  f = "GaView<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, "viewId") <- value
    object
  }
)

