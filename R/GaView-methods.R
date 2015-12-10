#' @include query-classes.R
#' @include management-api-classes.R
#' @include view-coerce.R
#' @include Query-generics.R
#' @importFrom methods setMethod as
NULL

# -- GaView ----
#' @describeIn GaView Returns the ID of the supplied view, or the first view
#'   within the supplied property or the first view within the first property of
#'   the supplied account, or get the view ID of the supplied query, or coerce a
#'   numeric or character into a viewId.
setMethod("GaView", "ANY", function(object) {as(object, "viewId")})

#' @describeIn GaView Replace the view being used by a query.
setMethod(
  f = "GaView<-",
  signature = c(".query", "ANY"),
  definition = function(object, value) {
    as(object, "viewId") <- value
    object
  }
)
