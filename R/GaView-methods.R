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

#' @describeIn GaView Select the default view of the property.
#' @examples
#' \dontrun{
#'    my_ga_account <- GaAccounts()[['60253332']]
#'    my_website_property <- my_ga_account$properties[['UA-60253332-2']]
#'    my_default_view <- GaView(my_website_property)
#' }
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

#' @describeIn GaView Select the default view of the first listed property of
#'   the account.
#' @examples
#' \dontrun{
#'    my_ga_account <- GaAccounts()[['60253332']]
#'    my_default_view <- GaView(my_ga_account)
#' }
setMethod(
  "GaView",
  signature = c("gaAccount", "missing"),
  definition = function(object) {
    GaView(object$properties$entities[[1]])
  }
)

#' @describeIn GaView Returns the ID of the supplied view, or the default view
#'   within the supplied property or the default view within the first property
#'   of the supplied account, or coerces a numeric or character into a
#'   \code{viewId}.
setMethod(
  "GaView",
  signature = c("ANY", "missing"),
  definition = function(object) {
    as(object, "viewId")
  }
)

#' @describeIn GaView gets the view ID of the supplied query.
setMethod(
  "GaView",
  signature = c(".query", "missing"),
  definition = function(object) {
    object@viewId
  }
)

#' @describeIn GaView Set the view of a query, returning the query with the
#'   updated view applied.
setMethod(
  "GaView",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, "viewId") <- value
    validObject(object)
    object
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

