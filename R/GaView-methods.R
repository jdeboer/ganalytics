#' @include query-classes.R
#' @include management-api-classes.R
#' @include view-coerce.R
#' @include GaView-generics.R
#' @importFrom methods setMethod as as<- callNextMethod
NULL

setMethod(
  f = "initialize",
  signature = "viewId",
  definition = function(.Object, value, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!missing(value)) {
      value <- sub(kGaPrefix, "ga:", value)
      value <- sapply(value, function(x) {
        if (!grepl("^ga:[0-9]+$", x)) {
          x <- paste("ga", x, sep = ":")
        }
        x
      })
      .Object@.Data <- unique(value)
      validObject(.Object)
    }
    .Object
  }
)

#' @describeIn GaView Select the default view of the property
setMethod(
  "GaView",
  signature = c("gaProperty", "missing"),
  definition = function(object) {
    defaultView <- object$defaultView
    if (length(defaultView) == 0) {
      defaultView <- object$views$entities[[1]]
    }
    as(defaultView, "viewId")
  }
)

#' @describeIn GaView Selects the first property of the account, which is then
#' used to select a view (as above).
setMethod(
  "GaView",
  signature = c("gaAccount", "missing"),
  definition = function(object) {
    GaView(object$properties$entities[[1]])
  }
)

# -- GaView ----
#' @describeIn GaView Returns the ID of the supplied view, or the first view
#'   within the supplied property or the first view within the first property of
#'   the supplied account, or coerce a numeric or character into a viewId.
setMethod("GaView", c("ANY", "missing"),
          function(object) {
            as(object, "viewId")
          }
        )

#' @describeIn GaView gets the view ID of the supplied query
setMethod("GaView", c(".query", "missing"),
          function(object) {
            object@viewId
          }
)

#' @describeIn GaView Set the view of a query, returning the query with the updated view applied.
setMethod("GaView", c(".query", "ANY"),
          function(object, value) {
            as(object, "viewId") <- value
          }
        )

#' @describeIn GaView Replaces the view being used by a query.
setMethod(
  f = "GaView<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    object@viewId <- as(value, "viewId")
    validObject(object)
    object
  }
)

